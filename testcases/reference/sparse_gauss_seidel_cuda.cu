#include <algorithm>
#include <chrono>
#include <cuda_runtime.h>
#include <cusparse.h>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <random>
#include <string>
#include <vector>

// 错误检查宏
#define CHECK_CUDA(func)                                                       \
  {                                                                            \
    cudaError_t status = (func);                                               \
    if (status != cudaSuccess) {                                               \
      printf("CUDA API failed at %s line %d with error: %s (%d)\n", __FILE__,  \
             __LINE__, cudaGetErrorString(status), status);                    \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
  }

#define CHECK_CUSPARSE(func)                                                   \
  {                                                                            \
    cusparseStatus_t status = (func);                                          \
    if (status != CUSPARSE_STATUS_SUCCESS) {                                   \
      printf("cuSPARSE API failed at %s line %d with error: %s (%d)\n",        \
             __FILE__, __LINE__, cusparseGetErrorString(status), status);      \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
  }

// 类型定义
using data_t = float;

// 常量定义
constexpr data_t EPS = 1e-3f;    // 收敛阈值
constexpr int MAX_ITER = 100000; // 最大迭代次数

// cuSPARSE CSR矩阵包装器
struct CUSPARSEMatrix {
  int rows;
  int cols;
  int nnz;                    // 非零元素数量
  std::vector<data_t> values; // 主机端数据
  std::vector<int> row_offsets;
  std::vector<int> col_indices;

  // 设备端数据
  data_t *d_values = nullptr;
  int *d_row_offsets = nullptr;
  int *d_col_indices = nullptr;

  // cuSPARSE描述符
  cusparseSpMatDescr_t mat_descr = nullptr;

  CUSPARSEMatrix(int r, int c) : rows(r), cols(c), nnz(0) {
    row_offsets.resize(rows + 1, 0);
  }

  ~CUSPARSEMatrix() {
    if (d_values)
      cudaFree(d_values);
    if (d_row_offsets)
      cudaFree(d_row_offsets);
    if (d_col_indices)
      cudaFree(d_col_indices);
    if (mat_descr)
      cusparseDestroySpMat(mat_descr);
  }

  void convertFromDense(const std::vector<data_t> &dense_matrix) {
    values.clear();
    col_indices.clear();
    row_offsets[0] = 0;

    for (int i = 0; i < rows; ++i) {
      bool has_diagonal = false;
      for (int j = 0; j < cols; ++j) {
        data_t val = dense_matrix[i * cols + j];
        if (std::abs(val) > EPS) {
          values.push_back(val);
          col_indices.push_back(j);
          if (i == j) {
            has_diagonal = true;
          }
        }
      }
      row_offsets[i + 1] = values.size();

      if (!has_diagonal) {
        std::cerr << "Error: Row " << i << " missing diagonal element!"
                  << std::endl;
        exit(1);
      }
    }
    nnz = values.size();

    std::cout << "Matrix info: " << rows << "x" << cols << ", NNZ: " << nnz
              << std::endl;
    std::cout << "Sparsity: " << (float)nnz / (rows * cols) * 100 << "%"
              << std::endl;
  }

  void moveToDevice() {
    CHECK_CUDA(cudaMalloc(&d_values, nnz * sizeof(data_t)));
    CHECK_CUDA(cudaMalloc(&d_row_offsets, (rows + 1) * sizeof(int)));
    CHECK_CUDA(cudaMalloc(&d_col_indices, nnz * sizeof(int)));

    CHECK_CUDA(cudaMemcpy(d_values, values.data(), nnz * sizeof(data_t),
                          cudaMemcpyHostToDevice));
    CHECK_CUDA(cudaMemcpy(d_row_offsets, row_offsets.data(),
                          (rows + 1) * sizeof(int), cudaMemcpyHostToDevice));
    CHECK_CUDA(cudaMemcpy(d_col_indices, col_indices.data(), nnz * sizeof(int),
                          cudaMemcpyHostToDevice));

    CHECK_CUSPARSE(cusparseCreateCsr(&mat_descr, rows, cols, nnz, d_row_offsets,
                                     d_col_indices, d_values,
                                     CUSPARSE_INDEX_32I, CUSPARSE_INDEX_32I,
                                     CUSPARSE_INDEX_BASE_ZERO, CUDA_R_32F));
  }

  void multiplyVector(const std::vector<data_t> &x, std::vector<data_t> &b) {
    for (int i = 0; i < rows; ++i) {
      b[i] = 0.0f;
      for (int j = row_offsets[i]; j < row_offsets[i + 1]; ++j) {
        b[i] += values[j] * x[col_indices[j]];
      }
    }
  }
};

// 生成随机稀疏矩阵
std::vector<data_t> generateRandomSparseMatrix(int N) {
  std::vector<data_t> matrix(N * N, 0.0f);
  std::mt19937 gen(13000);
  std::uniform_real_distribution<data_t> dis(-1.0f, 1.0f);

  // 设置目标非零元素数量（大约5%的稀疏度）
  int target_nnz = N * N / 20;
  int current_nnz = 0;

  // 先确保对角元素非零
  for (int i = 0; i < N; ++i) {
    matrix[i * N + i] = 10.0f + std::abs(dis(gen));
    current_nnz++;
  }

  // 随机填充剩余的非零元素
  while (current_nnz < target_nnz) {
    int i = gen() % N;
    int j = gen() % N;
    if (i != j && matrix[i * N + j] == 0.0f) {
      float val = dis(gen);
      matrix[i * N + j] = val;
      current_nnz++;
    }
  }

  // 确保对角占优
  for (int i = 0; i < N; ++i) {
    float row_sum = 0.0f;
    for (int j = 0; j < N; ++j) {
      if (i != j) {
        row_sum += std::abs(matrix[i * N + j]);
      }
    }
    if (row_sum >= std::abs(matrix[i * N + i])) {
      matrix[i * N + i] = row_sum + 1.0f;
    }
  }

  return matrix;
}

// 生成随机精确解向量
std::vector<data_t> generateExactSolution(int N) {
  std::vector<data_t> x(N);
  std::mt19937 gen(13000);
  std::uniform_real_distribution<data_t> dis(-4.0f, 4.0f);
  for (auto &val : x) {
    val = dis(gen);
  }
  return x;
}

// CUDA核函数
__global__ void gaussSeidelKernel(const int N, const data_t *A_values,
                                  const int *A_row_offsets,
                                  const int *A_col_indices, const data_t *b,
                                  data_t *x) {
  int row = blockIdx.x * blockDim.x + threadIdx.x;
  if (row >= N)
    return;

  data_t sum = 0.0f;
  data_t diag = 0.0f;

  // 找到对角元素位置
  int diag_pos = -1;
  for (int j = A_row_offsets[row]; j < A_row_offsets[row + 1]; j++) {
    if (A_col_indices[j] == row) {
      diag_pos = j;
      diag = A_values[j];
      break;
    }
  }

  if (diag_pos != -1) {
    // 计算非对角元素的和
    for (int j = A_row_offsets[row]; j < A_row_offsets[row + 1]; j++) {
      if (j != diag_pos) {
        int col = A_col_indices[j];
        sum += A_values[j] * x[col];
      }
    }
    // 更新x[row]
    x[row] = (b[row] - sum) / diag;
  }
}

__global__ void computeResidual(int N, const data_t *A_values,
                                const int *A_row_offsets,
                                const int *A_col_indices, const data_t *x,
                                const data_t *b, data_t *r) {
  int row = blockIdx.x * blockDim.x + threadIdx.x;
  if (row >= N)
    return;

  data_t sum = 0.0f;
  for (int j = A_row_offsets[row]; j < A_row_offsets[row + 1]; j++) {
    sum += A_values[j] * x[A_col_indices[j]];
  }
  r[row] = sum - b[row];
}

__global__ void computeResidualNorm(const data_t *r, data_t *norm, int size) {
  extern __shared__ data_t sdata[];
  int tid = threadIdx.x;
  int i = blockIdx.x * blockDim.x + threadIdx.x;

  sdata[tid] = 0.0f;
  if (i < size) {
    sdata[tid] = r[i] * r[i];
  }

  __syncthreads();

  for (int s = blockDim.x / 2; s > 0; s >>= 1) {
    if (tid < s) {
      sdata[tid] += sdata[tid + s];
    }
    __syncthreads();
  }

  if (tid == 0) {
    atomicAdd(norm, sdata[0]);
  }
}

int main(int argc, char **argv) {
  // 参数检查
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <matrix_dimension>" << std::endl;
    return 1;
  }

  // 解析矩阵维度参数
  int N = std::stoi(argv[1]);
  if (N <= 0) {
    std::cerr << "Matrix dimension must be positive!" << std::endl;
    return 1;
  }

  cusparseHandle_t cusparse_handle;
  CHECK_CUSPARSE(cusparseCreate(&cusparse_handle));

  // 生成随机稀疏矩阵
  std::vector<data_t> dense_matrix = generateRandomSparseMatrix(N);

  // 生成随机精确解向量
  std::vector<data_t> exact_solution = generateExactSolution(N);

  // 打印精确解的前10个元素
  std::cout << "Exact solution (first 10 elements):" << std::endl;
  for (int i = 0; i < std::min(10, N); ++i) {
    std::cout << std::fixed << std::setprecision(6) << exact_solution[i] << " ";
  }
  std::cout << std::endl << std::endl;

  // 转换为CSR格式并计算b = A * x_exact
  CUSPARSEMatrix A(N, N);
  A.convertFromDense(dense_matrix);
  std::vector<data_t> b(N);
  A.multiplyVector(exact_solution, b);
  A.moveToDevice();

  // 分配设备内存
  data_t *d_x, *d_b, *d_r, *d_norm;
  CHECK_CUDA(cudaMalloc(&d_x, N * sizeof(data_t)));
  CHECK_CUDA(cudaMalloc(&d_b, N * sizeof(data_t)));
  CHECK_CUDA(cudaMalloc(&d_r, N * sizeof(data_t)));
  CHECK_CUDA(cudaMalloc(&d_norm, sizeof(data_t)));

  // 初始化解向量和拷贝右端向量
  CHECK_CUDA(cudaMemset(d_x, 0, N * sizeof(data_t)));
  CHECK_CUDA(
      cudaMemcpy(d_b, b.data(), N * sizeof(data_t), cudaMemcpyHostToDevice));

  auto start = std::chrono::high_resolution_clock::now();
  int iter = 0;
  data_t norm = 1.0f;
  const int threads = 256;
  const int blocks = (N + threads - 1) / threads;

  while (norm > EPS && iter < MAX_ITER) {
    // 执行一次Gauss-Seidel迭代
    gaussSeidelKernel<<<blocks, threads>>>(N, A.d_values, A.d_row_offsets,
                                           A.d_col_indices, d_b, d_x);

    CHECK_CUDA(cudaDeviceSynchronize());

    // 计算残差
    computeResidual<<<blocks, threads>>>(N, A.d_values, A.d_row_offsets,
                                         A.d_col_indices, d_x, d_b, d_r);

    CHECK_CUDA(cudaDeviceSynchronize());

    // 计算残差范数
    CHECK_CUDA(cudaMemset(d_norm, 0, sizeof(data_t)));
    computeResidualNorm<<<blocks, threads, threads * sizeof(data_t)>>>(
        d_r, d_norm, N);

    CHECK_CUDA(
        cudaMemcpy(&norm, d_norm, sizeof(data_t), cudaMemcpyDeviceToHost));
    norm = std::sqrt(norm);

    std::cout << "Iteration " << iter << ", residual norm: " << norm
              << std::endl;
    iter++;
  }

  auto end = std::chrono::high_resolution_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::microseconds>(end - start);

  std::cout << "\nConverged in " << iter << " iterations, " << duration.count()
            << "us" << std::endl;
  std::cout << "Final residual norm: " << norm << std::endl;

  // 获取数值解
  std::vector<data_t> numerical_solution(N);
  CHECK_CUDA(cudaMemcpy(numerical_solution.data(), d_x, N * sizeof(data_t),
                        cudaMemcpyDeviceToHost));

  // 计算与精确解的平均误差
  data_t total_error = 0.0f;
  data_t max_error = 0.0f;
  for (int i = 0; i < N; ++i) {
    data_t error = std::abs(numerical_solution[i] - exact_solution[i]);
    total_error += error;
    max_error = std::max(max_error, error);
  }
  data_t avg_error = total_error / N;

  std::cout << "\nAverage error compared to exact solution: " << avg_error
            << std::endl;
  std::cout << "Maximum error compared to exact solution: " << max_error
            << std::endl;

  // 打印数值解的前10个元素
  std::cout << "\nNumerical solution (first 10 elements):" << std::endl;
  for (int i = 0; i < std::min(10, N); ++i) {
    std::cout << std::fixed << std::setprecision(6) << numerical_solution[i]
              << " ";
  }
  std::cout << std::endl;

  // 清理资源
  cusparseDestroy(cusparse_handle);

  cudaFree(d_x);
  cudaFree(d_b);
  cudaFree(d_r);
  cudaFree(d_norm);

  return 0;
}