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


def fib(n):
    a, b = 1, 1
    i = 0
    while i < n:
        i = i + 1
        a, b = b, a + b
    return a


class Wrapper_for_Model:
    def run(self):
        self._latcnt = 0
        self._obscnt = 0
        return self.Model()

    def Model(self):
        self._latcnt += 1
        r = pyro.sample("lat_" + str(self._latcnt), dist.Poisson(4.00000000))
        if (4 < r):
            l = 6
        else:
            self._latcnt += 1
            t = pyro.sample("lat_" + str(self._latcnt), dist.Poisson(4.00000000))
            _temp_0 = fib((3 * r))
            l = (t + _temp_0)
        self._obscnt += 1
        y = pyro.sample("obs_" + str(self._obscnt), dist.Poisson(l))
        return r
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
        r = self._ctrl_p.switch()
        if (4 < r):
            self._ctrl_p.switch(True)
            l = 6
        else:
            self._ctrl_p.switch(False)
            t = self._ctrl_p.switch()
            _temp_0 = fib((3 * r))
            l = (t + _temp_0)
        self._obscnt += 1
        return r

    def Proposal(self):
        self._latcnt += 1
        _temp_2 = pyro.sample("lat_" + str(self._latcnt), dist.Geometric(0.50000000))
        self._last_b = self._ctrl_m.switch(_temp_2)
        if self._last_b:
            self._ctrl_m.switch()
            return ()
        else:
            self._ctrl_m.switch()
            self._latcnt += 1
            _temp_1 = pyro.sample("lat_" + str(self._latcnt), dist.Geometric(0.50000000))
            self._last_b = self._ctrl_m.switch(_temp_1)
            return ()


def model():
    wm = Wrapper_for_Model()
    return wm.run()


def guide():
    pm = Proposal_for_Model()
    pm.run()


cond_model = pyro.condition(model, data={"obs_1": torch.tensor(6.0)})


_, gi = timeit(test, cond_model, guide)
print("generated code inference time: %fs" % (gi / 1e9))


def hmodel():
    r = pyro.sample("r", dist.Poisson(4.0))
    if 4 < r:
        l = 6
    else:
        t = pyro.sample("t", dist.Poisson(4.0))
        l = t + fib(3 * r)
    pyro.sample("y", dist.Poisson(l))
    return r
def hguide():
    r = pyro.sample("r", dist.Geometric(0.5))
    if 4 < r:
        return
    else:
        pyro.sample("t", dist.Geometric(0.5))
        return


cond_hmodel = pyro.condition(hmodel, data={"y": torch.tensor(6.0)})


_, gi = timeit(test, cond_hmodel, hguide)
print("hand-written code inference time: %fs" % (gi / 1e9))
