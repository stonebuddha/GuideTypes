#!/usr/bin/env python3

import torch
import pyro
import pyro.distributions as dist
from pyro.infer import EmpiricalMarginal, Importance
import pyro.poutine as poutine
from greenlet import greenlet
import time


def timeit(f, *args, **kwargs):
    t1 = time.time_ns()
    ret = f(*args, **kwargs)
    t2 = time.time_ns()
    return (ret, t2 - t1)


def test(cond_model, guide):
    importance = Importance(model=cond_model, guide=guide, num_samples=1000)
    imp_ret = importance.run()


identity_matrix2 = torch.eye(2)
def invert(m):
    return torch.inverse(m)
def mvn2(mu):
    def clo(cov):
        return dist.MultivariateNormal(mu, make_it_cov2(cov))
    return clo
def dirichlet3(alpha):
    return dist.Dirichlet(alpha)
def wishart2(nu):
    def clo(k):
        return dist.LKJCorrCholesky(k-1, torch.tensor(2.0))
    return clo
def make_it_cov2(res):
    res = torch.mm(res, res.t())
    res.add_(torch.eye(2))
    return res


class Wrapper_for_Model:
    def run(self):
        self._latcnt = 0
        self._obscnt = 0
        return self.Model()

    def Model(self):
        _temp_0 = dirichlet3(torch.stack([torch.tensor(1.00000000), torch.tensor(1.00000000), torch.tensor(1.00000000)]))
        self._latcnt += 1
        pi = pyro.sample("lat_" + str(self._latcnt), _temp_0)
        lambda_0 = identity_matrix2
        _temp_1 = wishart2(lambda_0)
        _temp_2 = _temp_1(3)
        self._latcnt += 1
        lambda_1 = pyro.sample("lat_" + str(self._latcnt), _temp_2)
        _temp_3 = wishart2(lambda_0)
        _temp_4 = _temp_3(3)
        self._latcnt += 1
        lambda_2 = pyro.sample("lat_" + str(self._latcnt), _temp_4)
        _temp_5 = wishart2(lambda_0)
        _temp_6 = _temp_5(3)
        self._latcnt += 1
        lambda_3 = pyro.sample("lat_" + str(self._latcnt), _temp_6)
        def _gensym_0(i):
            if (i == 0):
                return lambda_1
            else:
                if (i == 1):
                    return lambda_2
                else:
                    return lambda_3
        lambdas = _gensym_0
        mu_0 = torch.stack([torch.tensor(0.00000000), torch.tensor(0.00000000)])
        _temp_7 = mvn2(mu_0)
        _temp_8 = invert(lambda_1)
        _temp_9 = _temp_7(_temp_8)
        self._latcnt += 1
        mu_1 = pyro.sample("lat_" + str(self._latcnt), _temp_9)
        _temp_10 = mvn2(mu_0)
        _temp_11 = invert(lambda_2)
        _temp_12 = _temp_10(_temp_11)
        self._latcnt += 1
        mu_2 = pyro.sample("lat_" + str(self._latcnt), _temp_12)
        _temp_13 = mvn2(mu_0)
        _temp_14 = invert(lambda_3)
        _temp_15 = _temp_13(_temp_14)
        self._latcnt += 1
        mu_3 = pyro.sample("lat_" + str(self._latcnt), _temp_15)
        def _gensym_1(i):
            if (i == 0):
                return mu_1
            else:
                if (i == 1):
                    return mu_2
                else:
                    return mu_3
        mus = _gensym_1
        acc = ()
        for _ in range(100):
            self._latcnt += 1
            k = pyro.sample("lat_" + str(self._latcnt), dist.Categorical(pi))
            _temp_16 = mus(k)
            _temp_17 = mvn2(_temp_16)
            _temp_18 = lambdas(k)
            _temp_19 = invert(_temp_18)
            _temp_20 = _temp_17(_temp_19)
            self._obscnt += 1
            _temp_21 = pyro.sample("obs_" + str(self._obscnt), _temp_20)
            acc = ()
        _temp_22 = acc
        return ()
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
        _temp_0 = dirichlet3(torch.stack([torch.tensor(1.00000000), torch.tensor(1.00000000), torch.tensor(1.00000000)]))
        pi = self._ctrl_p.switch()
        lambda_0 = identity_matrix2
        _temp_1 = wishart2(lambda_0)
        _temp_2 = _temp_1(3)
        lambda_1 = self._ctrl_p.switch()
        _temp_3 = wishart2(lambda_0)
        _temp_4 = _temp_3(3)
        lambda_2 = self._ctrl_p.switch()
        _temp_5 = wishart2(lambda_0)
        _temp_6 = _temp_5(3)
        lambda_3 = self._ctrl_p.switch()
        def _gensym_0(i):
            if (i == 0):
                return lambda_1
            else:
                if (i == 1):
                    return lambda_2
                else:
                    return lambda_3
        lambdas = _gensym_0
        mu_0 = torch.stack([torch.tensor(0.00000000), torch.tensor(0.00000000)])
        _temp_7 = mvn2(mu_0)
        _temp_8 = invert(lambda_1)
        _temp_9 = _temp_7(_temp_8)
        mu_1 = self._ctrl_p.switch()
        _temp_10 = mvn2(mu_0)
        _temp_11 = invert(lambda_2)
        _temp_12 = _temp_10(_temp_11)
        mu_2 = self._ctrl_p.switch()
        _temp_13 = mvn2(mu_0)
        _temp_14 = invert(lambda_3)
        _temp_15 = _temp_13(_temp_14)
        mu_3 = self._ctrl_p.switch()
        def _gensym_1(i):
            if (i == 0):
                return mu_1
            else:
                if (i == 1):
                    return mu_2
                else:
                    return mu_3
        mus = _gensym_1
        acc = ()
        for _ in range(100):
            k = self._ctrl_p.switch()
            _temp_16 = mus(k)
            _temp_17 = mvn2(_temp_16)
            _temp_18 = lambdas(k)
            _temp_19 = invert(_temp_18)
            _temp_20 = _temp_17(_temp_19)
            self._obscnt += 1
            acc = ()
        _temp_22 = acc
        return ()

    def Proposal(self):
        _temp_23 = dirichlet3(torch.stack([torch.tensor(1.00000000), torch.tensor(1.00000000), torch.tensor(1.00000000)]))
        self._latcnt += 1
        pi = pyro.sample("lat_" + str(self._latcnt), _temp_23)
        self._last_b = self._ctrl_m.switch(pi)
        _temp_24 = wishart2(identity_matrix2)
        _temp_25 = _temp_24(3)
        self._latcnt += 1
        _temp_43 = pyro.sample("lat_" + str(self._latcnt), _temp_25)
        self._last_b = self._ctrl_m.switch(_temp_43)
        _temp_26 = wishart2(identity_matrix2)
        _temp_27 = _temp_26(3)
        self._latcnt += 1
        _temp_42 = pyro.sample("lat_" + str(self._latcnt), _temp_27)
        self._last_b = self._ctrl_m.switch(_temp_42)
        _temp_28 = wishart2(identity_matrix2)
        _temp_29 = _temp_28(3)
        self._latcnt += 1
        _temp_41 = pyro.sample("lat_" + str(self._latcnt), _temp_29)
        self._last_b = self._ctrl_m.switch(_temp_41)
        mu_0 = torch.stack([torch.tensor(0.00000000), torch.tensor(0.00000000)])
        _temp_30 = mvn2(mu_0)
        _temp_31 = _temp_30(identity_matrix2)
        self._latcnt += 1
        _temp_40 = pyro.sample("lat_" + str(self._latcnt), _temp_31)
        self._last_b = self._ctrl_m.switch(_temp_40)
        _temp_32 = mvn2(mu_0)
        _temp_33 = _temp_32(identity_matrix2)
        self._latcnt += 1
        _temp_39 = pyro.sample("lat_" + str(self._latcnt), _temp_33)
        self._last_b = self._ctrl_m.switch(_temp_39)
        _temp_34 = mvn2(mu_0)
        _temp_35 = _temp_34(identity_matrix2)
        self._latcnt += 1
        _temp_38 = pyro.sample("lat_" + str(self._latcnt), _temp_35)
        self._last_b = self._ctrl_m.switch(_temp_38)
        acc = ()
        for _ in range(100):
            self._latcnt += 1
            _temp_36 = pyro.sample("lat_" + str(self._latcnt), dist.Categorical(pi))
            self._last_b = self._ctrl_m.switch(_temp_36)
            acc = ()
        _temp_37 = acc
        return ()


def model():
    wm = Wrapper_for_Model()
    return wm.run()


def guide():
    pm = Proposal_for_Model()
    pm.run()


data_generator = poutine.trace(pyro.condition(model, data={"lat_5": torch.tensor([10., 4.]), "lab_6": torch.tensor([-4., -4.]), "lab_7": torch.tensor([4., -10.])}))
data_node = data_generator.get_trace().nodes
data = { "obs_" + str(i + 1): data_node["obs_" + str(i + 1)]["value"] for i in range(100) }


cond_model = pyro.condition(model, data=data)


_, gi = timeit(test, cond_model, guide)
print("generated code inference time: %fs" % (gi / 1e9))


def hmodel():
    pi = pyro.sample("pi", dirichlet3(torch.tensor([1.0, 1.0, 1.0])))
    lambda_0 = identity_matrix2
    lambda_1 = pyro.sample("lambda_1", wishart2(lambda_0)(3))
    lambda_2 = pyro.sample("lambda_2", wishart2(lambda_0)(3))
    lambda_3 = pyro.sample("lambda_3", wishart2(lambda_0)(3))
    def lambdas(i):
        if i == 0:
            return lambda_1
        elif i == 1:
            return lambda_2
        else:
            return lambda_3
    mu_0 = torch.tensor([0.0, 0.0])
    mu_1 = pyro.sample("mu_1", mvn2(mu_0)(invert(lambda_1)))
    mu_2 = pyro.sample("mu_2", mvn2(mu_0)(invert(lambda_2)))
    mu_3 = pyro.sample("mu_3", mvn2(mu_0)(invert(lambda_3)))
    def mus(i):
        if i == 0:
            return mu_1
        elif i == 1:
            return mu_2
        else:
            return mu_3
    for i in range(100):
        k = pyro.sample("k_" + str(i + 1), dist.Categorical(pi))
        pyro.sample("obs_" + str(i + 1), mvn2(mus(k))(invert(lambdas(k))))
def hguide():
    pi = pyro.sample("pi", dirichlet3(torch.tensor([1.0, 1.0, 1.0])))
    lambda_1 = pyro.sample("lambda_1", wishart2(identity_matrix2)(3))
    lambda_2 = pyro.sample("lambda_2", wishart2(identity_matrix2)(3))
    lambda_3 = pyro.sample("lambda_3", wishart2(identity_matrix2)(3))
    mu_0 = torch.tensor([0.0, 0.0])
    mu_1 = pyro.sample("mu_1", mvn2(mu_0)(identity_matrix2))
    mu_2 = pyro.sample("mu_2", mvn2(mu_0)(identity_matrix2))
    mu_3 = pyro.sample("mu_3", mvn2(mu_0)(identity_matrix2))
    for i in range(100):
        pyro.sample("k_" + str(i + 1), dist.Categorical(pi))


cond_hmodel = pyro.condition(hmodel, data={"z": torch.tensor(0.8)})


_, gi = timeit(test, cond_hmodel, hguide)
print("hand-written code inference time: %fs" % (gi / 1e9))
