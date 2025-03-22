// Author: Chu Xu
// Build with:
// /usr/local/cuda/bin/nvcc spmv.cu -o spmv -lcusparse -lnvidia-ml
// optionally, add -DN=xxx and -DNNZ=xxx

#include <assert.h>
#include <cuda_runtime.h>
#include <cusparse.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <nvml.h>

#ifndef N
#define N 2048
#endif

#ifndef NNZ
#define NNZ 204800
#endif

__host__ __device__ static uint64_t lfsr63(uint64_t x) {
  uint64_t bit = (x ^ (x >> 1)) & 1;
  return (x >> 1) | (bit << 62);
}

#define CHECK_CUDA(call)                                                       \
  do {                                                                         \
    cudaError_t err = call;                                                    \
    if (err != cudaSuccess) {                                                  \
      printf("CUDA error at %s:%d: %s\n", __FILE__, __LINE__,                  \
             cudaGetErrorString(err));                                         \
      exit(1);                                                                 \
    }                                                                          \
  } while (0)

#define CHECK_CUSPARSE(call)                                                   \
  do {                                                                         \
    cusparseStatus_t err = call;                                               \
    if (err != CUSPARSE_STATUS_SUCCESS) {                                      \
      printf("cuSPARSE error at %s:%d: %s\n", __FILE__, __LINE__,              \
             cusparseGetErrorString(err));                                     \
      exit(1);                                                                 \
    }                                                                          \
  } while (0)

#define CHECK_NVML(call)                                                       \
  do {                                                                         \
    nvmlReturn_t err = call;                                                   \
    if (err != NVML_SUCCESS) {                                                 \
      printf("NVML error at %s:%d: %s\n", __FILE__, __LINE__,                  \
             nvmlErrorString(err));                                            \
      exit(1);                                                                 \
    }                                                                          \
  } while (0)

int main() {
  // 创建输出文件
  FILE *fp = fopen("out_GPU.log", "w");
  if (fp == NULL) {
    printf("Error opening output file\n");
    return 1;
  }

  // NVML 初始化
  nvmlDevice_t device;
  CHECK_NVML(nvmlInit_v2());
  CHECK_NVML(nvmlDeviceGetHandleByIndex_v2(0, &device));

  // CPU内存分配和初始化
  float *h_val = new float[NNZ];
  uint32_t *h_idx = new uint32_t[NNZ];
  float *h_x = new float[N];
  uint32_t *h_ptr = new uint32_t[N + 1];
  float *h_y = new float[N];

  // 初始化数据（与原来相同的初始化过程）
  for (int i = 0; i < NNZ; i++) {
    h_val[i] = (float)i / (float)NNZ;
  }

  uint64_t seed = 1;
  for (int i = 0; i < NNZ; i++) {
    seed = lfsr63(seed);
    h_idx[i] = seed % N;
  }

  for (int i = 0; i < N; i++) {
    volatile float temp = (float)i / (float)N;
    h_x[i] = temp;
  }

  for (int i = 0; i < N; i++) {
    h_ptr[i] = i * (NNZ / N);
  }
  h_ptr[N] = NNZ;

  // GPU内存分配
  float *d_val, *d_x, *d_y;
  int *d_idx, *d_ptr;

  CHECK_CUDA(cudaMalloc(&d_val, NNZ * sizeof(float)));
  CHECK_CUDA(cudaMalloc(&d_idx, NNZ * sizeof(int)));
  CHECK_CUDA(cudaMalloc(&d_x, N * sizeof(float)));
  CHECK_CUDA(cudaMalloc(&d_ptr, (N + 1) * sizeof(int)));
  CHECK_CUDA(cudaMalloc(&d_y, N * sizeof(float)));

  // 创建临时缓冲区进行类型转换
  int *h_idx_int = new int[NNZ];
  int *h_ptr_int = new int[N + 1];

  // 将uint32_t转换为int
  for (int i = 0; i < NNZ; i++) {
    h_idx_int[i] = static_cast<int>(h_idx[i]);
  }
  for (int i = 0; i <= N; i++) {
    h_ptr_int[i] = static_cast<int>(h_ptr[i]);
  }

  // 数据传输到GPU
  CHECK_CUDA(
      cudaMemcpy(d_val, h_val, NNZ * sizeof(float), cudaMemcpyHostToDevice));
  CHECK_CUDA(
      cudaMemcpy(d_idx, h_idx_int, NNZ * sizeof(int), cudaMemcpyHostToDevice));
  CHECK_CUDA(cudaMemcpy(d_x, h_x, N * sizeof(float), cudaMemcpyHostToDevice));
  CHECK_CUDA(cudaMemcpy(d_ptr, h_ptr_int, (N + 1) * sizeof(int),
                        cudaMemcpyHostToDevice));

  // 创建cuSPARSE句柄
  cusparseHandle_t handle;
  CHECK_CUSPARSE(cusparseCreate(&handle));

  // 创建矩阵描述符（新版API）
  cusparseSpMatDescr_t matA;
  cusparseDnVecDescr_t vecX, vecY;
  void *dBuffer = NULL;
  size_t bufferSize = 0;
  float alpha = 1.0f;
  float beta = 0.0f;

  // 创建稀疏矩阵描述符
  CHECK_CUSPARSE(cusparseCreateCsr(&matA, N, N, NNZ, d_ptr, d_idx, d_val,
                                   CUSPARSE_INDEX_32I, CUSPARSE_INDEX_32I,
                                   CUSPARSE_INDEX_BASE_ZERO, CUDA_R_32F));

  // 创建密集向量描述符
  CHECK_CUSPARSE(cusparseCreateDnVec(&vecX, N, d_x, CUDA_R_32F));
  CHECK_CUSPARSE(cusparseCreateDnVec(&vecY, N, d_y, CUDA_R_32F));

  // 创建CUDA计时器
  cudaEvent_t start, stop;
  CHECK_CUDA(cudaEventCreate(&start));
  CHECK_CUDA(cudaEventCreate(&stop));

  // 获取缓冲区大小
  CHECK_CUSPARSE(cusparseSpMV_bufferSize(
      handle, CUSPARSE_OPERATION_NON_TRANSPOSE, &alpha, matA, vecX, &beta, vecY,
      CUDA_R_32F, CUSPARSE_SPMV_ALG_DEFAULT, &bufferSize));

  // 分配缓冲区
  CHECK_CUDA(cudaMalloc(&dBuffer, bufferSize));

  // 开始计时
  CHECK_CUDA(cudaEventRecord(start));

  // 执行SpMV计算
  CHECK_CUSPARSE(cusparseSpMV(handle, CUSPARSE_OPERATION_NON_TRANSPOSE, &alpha,
                              matA, vecX, &beta, vecY, CUDA_R_32F,
                              CUSPARSE_SPMV_ALG_DEFAULT, dBuffer));

  // 停止计时
  CHECK_CUDA(cudaEventRecord(stop));
  CHECK_CUDA(cudaEventSynchronize(stop));

  unsigned int power;
  CHECK_NVML(nvmlDeviceGetPowerUsage(device, &power));

  // 计算执行时间
  float milliseconds = 0;
  CHECK_CUDA(cudaEventElapsedTime(&milliseconds, start, stop));
  double seconds = milliseconds / 1000.0;

  // 将结果从GPU复制回CPU
  CHECK_CUDA(cudaMemcpy(h_y, d_y, N * sizeof(float), cudaMemcpyDeviceToHost));

  // 打印执行时间
  printf("Duration: %.10f s.\n", seconds);
  printf("Power: %.3f W.\n", (double)power / 1000);

  // 将结果写入文件
  fprintf(fp, "Result: [%f", h_y[0]);
  for (int i = 1; i < N; i++) {
    fprintf(fp, ", %f", h_y[i]);
  }
  fprintf(fp, "]\n");

  // 清理资源
  CHECK_CUSPARSE(cusparseDestroySpMat(matA));
  CHECK_CUSPARSE(cusparseDestroyDnVec(vecX));
  CHECK_CUSPARSE(cusparseDestroyDnVec(vecY));
  CHECK_CUSPARSE(cusparseDestroy(handle));

  if (dBuffer) {
    CHECK_CUDA(cudaFree(dBuffer));
  }

  fclose(fp);
  delete[] h_val;
  delete[] h_idx;
  delete[] h_x;
  delete[] h_ptr;
  delete[] h_y;
  delete[] h_idx_int;
  delete[] h_ptr_int;

  CHECK_CUDA(cudaFree(d_val));
  CHECK_CUDA(cudaFree(d_idx));
  CHECK_CUDA(cudaFree(d_x));
  CHECK_CUDA(cudaFree(d_ptr));
  CHECK_CUDA(cudaFree(d_y));

  CHECK_CUDA(cudaEventDestroy(start));
  CHECK_CUDA(cudaEventDestroy(stop));

  return 0;
}
