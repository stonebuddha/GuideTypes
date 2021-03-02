#!/usr/bin/env python3

import torch
import pyro
import pyro.distributions as dist
from pyro.infer import EmpiricalMarginal, Importance
from greenlet import greenlet
import time


def timeit(f, *args, **kwargs):
    t1 = time.time_ns()
    ret = f(*args, **kwargs)
    t2 = time.time_ns()
    return (ret, t2 - t1)


def test(cond_model, guide):
    importance = Importance(model=cond_model, guide=guide, num_samples=10000)
    emp_dist = EmpiricalMarginal(importance.run())


class Wrapper_for_Model:
    def run(self):
        self._latcnt = 0
        self._obscnt = 0
        return self.Model()

    def Model(self):
        self._latcnt += 1
        v = pyro.sample("lat_" + str(self._latcnt), dist.Gamma(2, 1))
        if (v < 2.00000000):
            self._obscnt += 1
            _temp_1 = pyro.sample("obs_" + str(self._obscnt), dist.Normal(-1.00000000, 1))
            return v
        else:
            self._latcnt += 1
            m = pyro.sample("lat_" + str(self._latcnt), dist.Beta(3, 1))
            self._obscnt += 1
            _temp_0 = pyro.sample("obs_" + str(self._obscnt), dist.Normal(m, 1))
            return v
class Proposal_for_Model:
    def run(self):
        self._latcnt = 0
        self._obscnt = 0
        def _wrap_m():
            return self.Model()
        def _wrap_p():
            return self.Proposal()
        self._ctrl_m = greenlet(_wrap_m)
        self._ctrl_p = greenlet(_wrap_p)
        self._last_b = None
        return self._ctrl_m.switch()

    def Model(self):
        v = self._ctrl_p.switch()
        if (v < 2.00000000):
            self._ctrl_p.switch(True)
            self._obscnt += 1
            return v
        else:
            self._ctrl_p.switch(False)
            m = self._ctrl_p.switch()
            self._obscnt += 1
            return v

    def Proposal(self):
        self._latcnt += 1
        v = pyro.sample("lat_" + str(self._latcnt), dist.Gamma(1, 1))
        self._last_b = self._ctrl_m.switch(v)
        if self._last_b:
            self._ctrl_m.switch()
            return ()
        else:
            self._ctrl_m.switch()
            self._latcnt += 1
            _temp_2 = pyro.sample("lat_" + str(self._latcnt), dist.Uniform(0., 1.))
            self._last_b = self._ctrl_m.switch(_temp_2)
            return ()


def model():
    wm = Wrapper_for_Model()
    return wm.run()


def guide():
    pm = Proposal_for_Model()
    pm.run()


cond_model = pyro.condition(model, data={"obs_1": torch.tensor(0.8)})


_, gi = timeit(test, cond_model, guide)
print("generated code inference time: %fs" % (gi / 1e9))


def hmodel():
    v = pyro.sample("x", dist.Gamma(2, 1))
    if v < 2:
        pyro.sample("z", dist.Normal(-1, 1))
        return v
    else:
        m = pyro.sample("y", dist.Beta(3, 1))
        pyro.sample("z", dist.Normal(m, 1))
        return v
def hguide():
    v = pyro.sample("x", dist.Gamma(1, 1))
    if v < 2:
        return
    else:
        pyro.sample("y", dist.Uniform(0, 1))
        return


cond_hmodel = pyro.condition(hmodel, data={"z": torch.tensor(0.8)})


_, gi = timeit(test, cond_hmodel, hguide)
print("hand-written code inference time: %fs" % (gi / 1e9))
