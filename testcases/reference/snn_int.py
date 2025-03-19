# from http://genn-team.github.io/genn/documentation/5/tutorials/comp_neuro_101/1_neurons.html#Build-model
import numpy as np
import matplotlib.pyplot as plt

import pygenn
from pygenn import GeNNModel

model = GeNNModel("float", "izhikevich")
model.dt = 1.0 # scaled by 10

neuron_params = {
}

neuron_vars = {
    "V": -65 * 100, # scaled by 100
    "U": -20 * 1000, # scaled by 1000
    "a": [2, 10, 2, 2], # scaled by 100
    "b": [2, 2, 2, 2], # scaled by 10
    "c": [-65, -65, -50, -55],
    "d": [8, 2, 2, 4],
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


pop = model.add_neuron_population(
    "Neurons", 4, neuron_model, neuron_params, neuron_vars
)

model.add_current_source("CurrentSource", "DC", pop, {"amp": 10.0}, {})

model.build()
model.load()

voltage = pop.vars["V"]

voltages = []
while model.t < 2000.0:
    model.step_time()
    voltage.pull_from_device()
    voltages.append(voltage.values)

# Stack voltages together into a 2000x4 matrix
voltages = np.vstack(voltages)

# Create figure with 4 axes
fig, axes = plt.subplots(4, sharex=True, figsize=(15, 8))

# Plot voltages of each neuron in
for i, t in enumerate(["RS", "FS", "CH", "IB"]):
    axes[i].set_title(t)
    axes[i].set_ylabel("V [mV]")
    axes[i].plot(np.arange(0.0, 200.0, 0.1), voltages[:, i])

axes[-1].set_xlabel("Time [ms]")

plt.savefig("plot.png")
