#!/usr/bin/env python3

import torch
from torch.distributions import constraints
import pyro
import pyro.distributions as dist
from pyro.infer import SVI, Trace_ELBO
import pyro.optim as optim
from greenlet import greenlet
import time


def timeit(f, *args, **kwargs):
    t1 = time.time_ns()
    ret = f(*args, **kwargs)
    t2 = time.time_ns()
    return (ret, t2 - t1)


def test(cond_model, guide):
    pyro.clear_param_store()
    svi = SVI(model=cond_model, guide=guide, optim=optim.SGD({"lr": 0.001, "momentum":0.1}), loss=Trace_ELBO())

    losses = []
    num_steps = 2500
    for t in range(num_steps):
        losses.append(svi.step())


class Wrapper_for_Model:
    def run(self):
        self._latcnt = 0
        self._obscnt = 0
        return self.Model()

    def Model(self):
        self._latcnt += 1
        weight = pyro.sample("lat_" + str(self._latcnt), dist.Normal(8.50000000, 1))
        self._obscnt += 1
        y = pyro.sample("obs_" + str(self._obscnt), dist.Normal(weight, 0.75000000))
        return weight
class Proposal_for_Model:
    def run(self, _p_a, _p_b):
        self._latcnt = 0
        self._obscnt = 0
        def _wrap_m():
            return self.Model()
        def _wrap_p():
            return self.Guide(_p_a, _p_b)
        self._ctrl_m = greenlet(_wrap_m)
        self._ctrl_p = greenlet(_wrap_p)
        self._last_b = None
        return self._ctrl_m.switch()

    def Model(self):
        weight = self._ctrl_p.switch()
        self._obscnt += 1
        return weight

    def Guide(self, a, b):
        self._latcnt += 1
        weight = pyro.sample("lat_" + str(self._latcnt), dist.Normal(a, b))
        self._last_b = self._ctrl_m.switch(weight)
        return ()


def model():
    wm = Wrapper_for_Model()
    return wm.run()


def guide():
    p_a = pyro.param("a", torch.tensor(8.5))
    p_b = pyro.param("b", torch.tensor(1.), constraint=constraints.positive)
    pm = Proposal_for_Model()
    pm.run(p_a, p_b)


cond_model = pyro.condition(model, data={"obs_1": torch.tensor(9.5)})


_, gi = timeit(test, cond_model, guide)
print("generated code inference time: %fs" % (gi / 1e9))


def hmodel():
    weight = pyro.sample("weight", dist.Normal(8.5, 1.))
    return pyro.sample("measurement", dist.Normal(weight, 0.75))
def hguide():
    a = pyro.param("a", torch.tensor(8.5))
    b = pyro.param("b", torch.tensor(1.), constraint=constraints.positive)
    return pyro.sample("weight", dist.Normal(a, b))


cond_hmodel = pyro.condition(hmodel, data={"measurement": torch.tensor(9.5)})


_, gi = timeit(test, cond_hmodel, hguide)
print("hand-written code inference time: %fs" % (gi / 1e9))
