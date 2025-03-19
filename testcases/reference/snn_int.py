# from http://genn-team.github.io/genn/documentation/5/tutorials/comp_neuro_101/1_neurons.html#Build-model
import numpy as np
import matplotlib.pyplot as plt

import pygenn
from pygenn import (
    GeNNModel,
    init_weight_update,
    init_postsynaptic,
    init_sparse_connectivity,
)

model = GeNNModel("float", "izhikevich")
model.dt = 1.0  # scaled by 10

neuron_params = {}

neuron_vars = {
    "V": -65 * 100,  # scaled by 100
    "U": -20 * 1000,  # scaled by 1000
    "a": 2,  # scaled by 100
    "b": 2,  # scaled by 10
    "c": -65,
    "d": 8,
}

# from http://genn-team.github.io/genn/documentation/5/custom_models.html

neuron_model = pygenn.create_neuron_model(
    "izhikevich",
    sim_code="""
        int iV = (int)V;
        int vscale = 100;
        int uscale = 1000;
        int ascale = 100;
        int bscale = 10;
        int iU = (int)U;
        int idt = (int)dt;
        int iIsyn = (int)Isyn;
        int ia = (int)a;
        int ib = (int)b;
        int ic = (int)c;
        int id = (int)d;
        if (iV >= 30 * vscale) {
            iV = ic * vscale;
            iU += id * uscale;
        }
        iV = iV + idt * (iV * iV / vscale / 25 + 5 * iV + 140 * vscale - iU * vscale / uscale + iIsyn * vscale) / 10;
        iU = iU + (ib * iV * uscale / vscale / 10 - iU) * ia * idt / bscale / ascale;
        if (iV >= 30 * vscale) {
            iV = 30 * vscale;
        }
        V = iV;
        U = iU;
        """,
    threshold_condition_code="V >= 30",
    reset_code="""
        """,
    vars=[
        ("V", "scalar"),
        ("U", "scalar"),
        ("a", "scalar"),
        ("b", "scalar"),
        ("c", "scalar"),
        ("d", "scalar"),
    ],
)

neurons_per_population = 2
timesteps = 30

pop1 = model.add_neuron_population(
    "Neurons1", neurons_per_population, neuron_model, neuron_params, neuron_vars
)
pop1.spike_recording_enabled = True

pop2 = model.add_neuron_population(
    "Neurons2", neurons_per_population, neuron_model, neuron_params, neuron_vars
)
pop2.spike_recording_enabled = True

sources_ini = {"startSpike": [0], "endSpike": [timesteps // 10]}
sources = model.add_neuron_population("Sources", 1, "SpikeSourceArray", {}, sources_ini)
sources.extra_global_params["spikeTimes"].set_init_values(np.arange(0, timesteps, 10))

# fully connected
model.add_synapse_population(
    "SourceToPop1",
    "SPARSE",
    sources,
    pop1,
    init_weight_update("StaticPulseConstantWeight", {"g": 100.0}),
    init_postsynaptic("DeltaCurr", {}),
    init_sparse_connectivity("FixedProbability", {"prob": 1.0}),
)

custom_connect = pygenn.create_sparse_connect_init_snippet(
    "custom_connect",
    params=[("prob", "unsigned int")],
    row_build_code="""
        for(unsigned int i = 0; i < num_post; i++) {
            unsigned int x = id_pre * 2654435761;
            x ^= i * 2654435761;
            unsigned rand = x & 32767;
            if (rand < 32767 / prob) {
                addSynapse(i + id_post_begin);
            }
        }
        """,
    calc_max_row_len_func=lambda num_pre, num_post, pars: neurons_per_population,
    calc_max_col_len_func=lambda num_pre, num_post, pars: neurons_per_population,
)

model.add_synapse_population(
    "Pop1ToPop2",
    "SPARSE",
    pop1,
    pop2,
    init_weight_update("StaticPulseConstantWeight", {"g": 30.0}),
    init_postsynaptic("DeltaCurr", {}),
    init_sparse_connectivity(custom_connect, {"prob": 10}),
)

model.add_synapse_population(
    "Pop2ToPop1",
    "SPARSE",
    pop2,
    pop1,
    init_weight_update("StaticPulseConstantWeight", {"g": 30.0}),
    init_postsynaptic("DeltaCurr", {}),
    init_sparse_connectivity(custom_connect, {"prob": 10}),
)


model.build()
model.load(num_recording_timesteps=timesteps)

voltage = pop1.vars["V"]

voltages = []
while model.t < timesteps:
    model.step_time()
    voltage.pull_from_device()
    voltages.append(voltage.values)

model.pull_recording_buffers_from_device()

print(
    "Fire count: ",
    len(pop1.spike_recording_data[0][0]) + len(pop2.spike_recording_data[0][0]),
)

# Stack voltages together into a 2000x4 matrix
voltages = np.vstack(voltages)
print(voltages)

# Create figure with 4 axes
fig, axes = plt.subplots(neurons_per_population, sharex=True, figsize=(15, 8))

# Plot voltages of each neuron in
for i in range(neurons_per_population):
    axes[i].set_ylabel("V [mV]")
    axes[i].plot(np.arange(0.0, timesteps, model.dt), voltages[:, i])

axes[-1].set_xlabel("Time [ms]")

plt.savefig("plot.png")
