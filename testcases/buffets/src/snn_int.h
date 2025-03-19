#include "common.h"

int N;
int S;
int P;
int T;
int dt;

struct Synapse {
  int target_neuron;
  int w; // weight
  int d; // delay
};

struct SpikeSource {
  struct Synapse *synapses;
  int num_synapses;
};

struct Neuron {
  // variables
  int v;
  int u;
  int a;
  int b;
  int c;
  int d;

  struct Synapse *synapses;
  int num_synapses;
};

#ifndef NEURONS_PER_POPULATION
#define NEURONS_PER_POPULATION 10
#endif

// [time][neuron]
#define MAX_DELAY 10
#define MAX_NEURON 2048
#define MAX_SOURCES 1024
#define MAX_SYNAPSES 262144
int input[MAX_DELAY + 1][MAX_NEURON];

struct Neuron neurons[MAX_NEURON];
struct SpikeSource spike_sources[MAX_SOURCES];
struct Synapse synapses[MAX_SYNAPSES];

int max_delay = 0;
int vscale = 100;
int uscale = 1000;
int ascale = 100;
int bscale = 10;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

int main(int hartid) {
  if (hartid != 0)
    spin();
  N = 0;         // number of neurons
  P = 0;         // number of spike sources
  S = 0;         // number of synapses
  T = 100;       // number of timesteps
  max_delay = 1; // maximum delay is 1
  dt = 1;

  int N1 = NEURONS_PER_POPULATION; // number of neurons in one population
  int prob = 100;                  // probability of synapse connection (1/10)

  // init data
  printf_("Initialize data\r\n");

  // first population
  for (int n = 0; n < N1; n++) {
    neurons[N].v = -65 * 100;
    neurons[N].u = -20 * 1000;
    neurons[N].a = 2;
    neurons[N].b = 2;
    neurons[N].c = -65;
    neurons[N].d = 8;

    neurons[N].synapses = &synapses[S];
    neurons[N].num_synapses = 0;
    for (int m = 0; m < N1; m++) {
      unsigned int x = n * 2654435761;
      x ^= m * 2654435761;
      unsigned rand = x & 32767;
      if (rand < 32767 / prob) {
        // make connection
        neurons[N].num_synapses++;
        // connection to second population
        synapses[S].target_neuron = m + N1;
        synapses[S].w = 30;
        synapses[S].d = 1;
        S++;
      }
    }

    N++;
  }

  // second population
  for (int n = 0; n < N1; n++) {
    neurons[N].v = -65 * 100;
    neurons[N].u = -20 * 1000;
    neurons[N].a = 2;
    neurons[N].b = 2;
    neurons[N].c = -65;
    neurons[N].d = 8;

    neurons[N].synapses = &synapses[S];
    neurons[N].num_synapses = 0;
    for (int m = 0; m < N1; m++) {
      unsigned int x = n * 2654435761;
      x ^= m * 2654435761;
      unsigned rand = x & 32767;
      if (rand < 32767 / prob) {
        // make connection
        neurons[N].num_synapses++;
        // connection to first population
        synapses[S].target_neuron = m;
        synapses[S].w = 30;
        synapses[S].d = 1;
        S++;
      }
    }

    N++;
  }

  // create one spike source to all first population
  spike_sources[P].synapses = &synapses[S];
  spike_sources[P].num_synapses = 0;
  for (int m = 0; m < N1; m++) {
    // make connection
    spike_sources[P].num_synapses++;
    // connection to first population
    synapses[S].target_neuron = m;
    synapses[S].w = 100;
    synapses[S].d = 1;
    S++;
  }
  P++;

  // clear input
  for (int i = 0; i <= max_delay; i++) {
    for (int n = 0; n < N; n++) {
      input[i][n] = 0;
    }
  }

  assert(N <= MAX_NEURON);

  printf_("Got %d neurons, %d synapses, %d spike sources, %d timesteps\r\n", N,
          S, P, T);

  unsigned long before = read_csr(mcycle);
  int sum_fire_count = 0;
  for (int t = 0; t < T; t++) {
    // printf_("Timestep %d begin\r\n", t);
    int fire_count = 0;
    // timestep

    // check spike sources
    for (int s = 0; s < P; s++) {
      if (t % 10 == 0) {
        // add event
        for (int i = 0; i < spike_sources[s].num_synapses; i++) {
          input[(t + spike_sources[s].synapses[i].d) % (max_delay + 1)]
               [spike_sources[s].synapses[i].target_neuron] +=
              spike_sources[s].synapses[i].w;
        }
      }
    }

    // update neuron
    for (int n = 0; n < N; n++) {
      int old_v = neurons[n].v;
      int old_u = neurons[n].u;
      int new_v = old_v;
      int new_u = old_u;
      if (new_v >= 30 * vscale) {
        new_v = neurons[n].c * vscale;
        new_u += neurons[n].d * uscale;
      }
      new_v = new_v + dt *
                          (new_v * new_v / vscale / 25 + 5 * new_v +
                           140 * vscale - new_u * vscale / uscale +
                           input[t % (max_delay + 1)][n] * vscale) /
                          10;
      new_u = new_u + (neurons[n].b * new_v * uscale / vscale / 10 - new_u) *
                          neurons[n].a * dt / bscale / ascale;

      input[t % (max_delay + 1)][n] = 0.0;

      if (new_v >= 30 * vscale) {
        new_v = 30 * vscale;

        fire_count++;
        // fire
        for (int i = 0; i < neurons[n].num_synapses; i++) {
          input[(t + neurons[n].synapses[i].d) % (max_delay + 1)]
               [neurons[n].synapses[i].target_neuron] +=
              neurons[n].synapses[i].w;
        }
      }

      neurons[n].v = new_v;
      neurons[n].u = new_u;
    }

    // printf_("Timestep %d fire %d times\r\n", t, fire_count);
    sum_fire_count += fire_count;
  }
  unsigned elapsed = read_csr(mcycle) - before;

  printf_("Cycles: %d\n", elapsed);
  printf_("Fire rate: %d\n", sum_fire_count);
  return 0;
}