#include <stdio.h>

#define printf_ printf

int N;
int S;
int P;
int T;
int dt;

struct Synapse {
  int target_neuron;
  int w;
  int d;
};

struct SpikeSource {
  int r;
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
#define MAX_TIME 10000
#define MAX_NEURON 1024
#define MAX_SOURCES 1024
#define MAX_SYNAPSES 1024
int input[MAX_TIME][MAX_NEURON] = {0};

struct Neuron neurons[MAX_NEURON];
struct SpikeSource spike_sources[MAX_SOURCES];
struct Synapse synapses[MAX_SYNAPSES];

int max_delay = 0;

static unsigned long next = 1;

/* RAND_MAX assumed to be 32767 */
const int RAND_MAX = 32767;
int myrand(void) {
  next = next * 1103515245 + 12345;
  return ((unsigned)(next / 65536) % 32768);
}

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

int main(int argc, char *argv[]) {
  N = 0;         // number of neurons
  P = 0;         // number of spike sources
  S = 0;         // number of synapses
  T = 100;       // number of timesteps
  max_delay = 1; // maximum delay is 1

  int N1 = NEURONS_PER_POPULATION; // number of neurons in one population
  int prob = 10;                   // probability of synapse connection (1/10)

  // init data
  printf_("Initialize data\r\n");

  // first population
  for (int n = 0; n < N1; n++) {
    neurons[N].synapses = &synapses[S];
    neurons[N].num_synapses = 0;
    for (int m = 0; m < N1; m++) {
      if (myrand() < RAND_MAX / prob) {
        // make connection
        neurons[N].num_synapses++;
        // connection to second population
        synapses[S].target_neuron = m + N1;
        synapses[S].d = 1;
        synapses[S].w = 30;
        S++;
      }
    }

    N++;
  }

  // second population
  for (int n = 0; n < N1; n++) {
    neurons[N].synapses = &synapses[S];
    neurons[N].num_synapses = 0;
    for (int m = 0; m < N1; m++) {
      if (myrand() < RAND_MAX / prob) {
        // make connection
        neurons[N].num_synapses++;
        // connection to first population
        synapses[S].target_neuron = m;
        synapses[S].d = 1;
        synapses[S].w = 30;
        S++;
      }
    }

    N++;
  }

  // create one spike source to all first population
  spike_sources[P].r = RAND_MAX / 3;
  spike_sources[P].num_synapses = 0;
  spike_sources[P].synapses = &synapses[S];
  for (int m = 0; m < N1; m++) {
    // make connection
    spike_sources[P].num_synapses++;
    // connection to first population
    synapses[S].target_neuron = m;
    synapses[S].d = 1;
    synapses[S].w = 30;
    S++;
  }
  P++;

  printf_("Got %d neurons, %d synapses, %d spike sources, %d timesteps\r\n", N,
          S, P, T);
  int sum_fire_count = 0;
  for (int t = 1; t <= T; t++) {
    printf_("Timestep %d begin\r\n", t);
    int fire_count = 0;
    // timestep

    // check spike sources
    for (int s = 0; s < P; s++) {
      if (spike_sources[s].r > myrand()) {
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
      int new_v = old_v + dt * (old_v * old_v / 25 + 5 * old_v + 140 - old_u) +
                  input[t % (max_delay + 1)][n];
      int new_u = old_u + dt * neurons[n].a * (neurons[n].b * old_v - old_u);

      input[t % (max_delay + 1)][n] = 0.0;

      if (new_v >= 30) {
        new_v = neurons[n].c;
        new_u = new_u + neurons[n].d;

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

    printf_("Timestep %d fire %d times\r\n", t, fire_count);
    sum_fire_count += fire_count;
  }

  printf_("Fire rate: %d\n", sum_fire_count);
  return 0;
}