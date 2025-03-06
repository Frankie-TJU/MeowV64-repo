// for poisson
typedef float data_t;
const size_t GROUP_LEN = 8;
#ifdef N_OVERRIDE
const size_t WIDTH = N_OVERRIDE;
const size_t HEIGHT = N_OVERRIDE;
#else
const size_t WIDTH = 16;
const size_t HEIGHT = 16;
#endif
const data_t EPS = 1e-6;
const data_t EARLY_EPS = 1e-6;
const double FREQ = 500000000.0;
