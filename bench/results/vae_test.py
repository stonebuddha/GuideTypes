#!/usr/bin/env python3

import torch
from torch.distributions import constraints
import torchvision.datasets as dset
import torch.nn as nn
import torchvision.transforms as transforms
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
    svi = SVI(model=cond_model, guide=guide, optim=optim.Adam({"lr": 1.0e-3}), loss=Trace_ELBO())

    losses = []
    num_steps = 100
    for t in range(num_steps):
        losses.append(svi.step())


def setup_data_loaders(batch_size=128, use_cuda=False):
    root = './data'
    download = True
    trans = transforms.ToTensor()
    train_set = dset.MNIST(root=root, train=True, transform=trans,
                           download=download)
    test_set = dset.MNIST(root=root, train=False, transform=trans)

    kwargs = {'num_workers': 0, 'pin_memory': use_cuda}
    train_loader = torch.utils.data.DataLoader(dataset=train_set,
        batch_size=batch_size, shuffle=True, **kwargs)
    test_loader = torch.utils.data.DataLoader(dataset=test_set,
        batch_size=batch_size, shuffle=False, **kwargs)
    return train_loader, test_loader


train_loader, test_loader = setup_data_loaders(batch_size=256)
for x, _ in train_loader:
    break
x = x.reshape(-1, 784)
data = { "obs_" + str(i + 1): x[i] for i in range(256) }


zeros50 = torch.zeros(50)
ones50 = torch.ones(50)
def mulmv400x50(m):
    def clo(v):
        return m.mv(v)
    return clo
def mulmv784x400(m):
    def clo(v):
        return m.mv(v)
    return clo
def softplus400(v):
    return nn.Softplus()(v)
def sigmoid784(v):
    return nn.Sigmoid()(v)
def mulmv400x784(m):
    def clo(v):
        return m.mv(v)
    return clo
def mulmv50x400(m):
    def clo(v):
        return m.mv(v)
    return clo
def exp50(v):
    return torch.exp(v)


class Wrapper_for_Model:
    def run(self, m_fc1, m_fc1_b, m_fc21, m_fc21_b):
        self._latcnt = 0
        self._obscnt = 0
        return self.Model(m_fc1, m_fc1_b, m_fc21, m_fc21_b)

    def Model(self, m_fc1, m_fc1_b, m_fc21, m_fc21_b):
        acc = ()
        for _ in range(256):
            z_loc = zeros50
            z_scale = ones50
            self._latcnt += 1
            z = pyro.sample("lat_" + str(self._latcnt), dist.Normal(z_loc, z_scale))
            _temp_0 = mulmv400x50(m_fc1)
            _temp_1 = _temp_0(z)
            hidden = softplus400((_temp_1 + m_fc1_b))
            _temp_2 = mulmv784x400(m_fc21)
            _temp_3 = _temp_2(hidden)
            loc_img = sigmoid784((_temp_3 + m_fc21_b))
            self._obscnt += 1
            _temp_4 = pyro.sample("obs_" + str(self._obscnt), dist.Bernoulli(loc_img))
            acc = ()
        _temp_5 = acc
        return ()
class Proposal_for_Model:
    def run(self, _m_m_fc1, _m_m_fc1_b, _m_m_fc21, _m_m_fc21_b, _p_g_fc1, _p_g_fc1_b, _p_g_fc21, _p_g_fc21_b, _p_g_fc22, _p_g_fc22_b, _p_xs):
        self._latcnt = 0
        self._obscnt = 0
        def _wrap_m():
            return self.Model(_m_m_fc1, _m_m_fc1_b, _m_m_fc21, _m_m_fc21_b)
        def _wrap_p():
            return self.Guide(_p_g_fc1, _p_g_fc1_b, _p_g_fc21, _p_g_fc21_b, _p_g_fc22, _p_g_fc22_b, _p_xs)
        self._ctrl_m = greenlet(_wrap_m)
        self._ctrl_p = greenlet(_wrap_p)
        self._last_b = None
        return self._ctrl_m.switch()

    def Model(self, m_fc1, m_fc1_b, m_fc21, m_fc21_b):
        acc = ()
        for _ in range(256):
            z_loc = zeros50
            z_scale = ones50
            z = self._ctrl_p.switch()
            _temp_0 = mulmv400x50(m_fc1)
            _temp_1 = _temp_0(z)
            hidden = softplus400((_temp_1 + m_fc1_b))
            _temp_2 = mulmv784x400(m_fc21)
            _temp_3 = _temp_2(hidden)
            loc_img = sigmoid784((_temp_3 + m_fc21_b))
            self._obscnt += 1
            acc = ()
        _temp_5 = acc
        return ()

    def Guide(self, g_fc1, g_fc1_b, g_fc21, g_fc21_b, g_fc22, g_fc22_b, xs):
        acc = ()
        for x in xs:
            _temp_6 = mulmv400x784(g_fc1)
            _temp_7 = _temp_6(x)
            hidden = softplus400((_temp_7 + g_fc1_b))
            _temp_8 = mulmv50x400(g_fc21)
            _temp_9 = _temp_8(hidden)
            z_loc = (_temp_9 + g_fc21_b)
            _temp_10 = mulmv50x400(g_fc22)
            _temp_11 = _temp_10(hidden)
            z_scale = exp50((_temp_11 + g_fc22_b))
            self._latcnt += 1
            _temp_12 = pyro.sample("lat_" + str(self._latcnt), dist.Normal(z_loc, z_scale))
            self._last_b = self._ctrl_m.switch(_temp_12)
            acc = ()
        _temp_13 = acc
        return ()


def model():
    m_fc1 = pyro.param("m_fc1", torch.zeros((400, 50)))
    m_fc1_b = pyro.param("m_fc1_b", torch.zeros(400))
    m_fc21 = pyro.param("m_fc21", torch.zeros((784, 400)))
    m_fc21_b = pyro.param("m_fc21_b", torch.zeros(784))
    wm = Wrapper_for_Model()
    wm.run(m_fc1, m_fc1_b, m_fc21, m_fc21_b)


def guide():
    m_fc1 = pyro.param("m_fc1", torch.zeros((400, 50)))
    m_fc1_b = pyro.param("m_fc1_b", torch.zeros(400))
    m_fc21 = pyro.param("m_fc21", torch.zeros((784, 400)))
    m_fc21_b = pyro.param("m_fc21_b", torch.zeros(784))
    g_fc1 = pyro.param("g_fc1", torch.zeros((400, 784)))
    g_fc1_b = pyro.param("g_fc1_b", torch.zeros(400))
    g_fc21 = pyro.param("g_fc21", torch.zeros((50, 400)))
    g_fc21_b = pyro.param("g_fc21_b", torch.zeros(50))
    g_fc22 = pyro.param("g_fc22", torch.zeros((50, 400)))
    g_fc22_b = pyro.param("g_fc22_b", torch.zeros(50))
    pm = Proposal_for_Model()
    pm.run(m_fc1, m_fc1_b, m_fc21, m_fc21_b, g_fc1, g_fc1_b, g_fc21, g_fc21_b, g_fc22, g_fc22_b, x)


cond_model = pyro.condition(model, data=data)


_, gi = timeit(test, cond_model, guide)
print("generated code inference time: %fs" % (gi / 1e9))


def hmodel():
    m_fc1 = pyro.param("m_fc1", torch.zeros((400, 50)))
    m_fc1_b = pyro.param("m_fc1_b", torch.zeros(400))
    m_fc21 = pyro.param("m_fc21", torch.zeros((784, 400)))
    m_fc21_b = pyro.param("m_fc21_b", torch.zeros(784))
    for i in range(256):
        z_loc = zeros50
        z_scale = ones50
        z = pyro.sample("z_" + str(i + 1), dist.Normal(z_loc, z_scale))
        hidden = softplus400(mulmv400x50(m_fc1)(z) + m_fc1_b)
        loc_img = sigmoid784(mulmv784x400(m_fc21)(hidden) + m_fc21_b)
        pyro.sample("obs_" + str(i + 1), dist.Bernoulli(loc_img))
def hguide():
    g_fc1 = pyro.param("g_fc1", torch.zeros((400, 784)))
    g_fc1_b = pyro.param("g_fc1_b", torch.zeros(400))
    g_fc21 = pyro.param("g_fc21", torch.zeros((50, 400)))
    g_fc21_b = pyro.param("g_fc21_b", torch.zeros(50))
    g_fc22 = pyro.param("g_fc22", torch.zeros((50, 400)))
    g_fc22_b = pyro.param("g_fc22_b", torch.zeros(50))
    i = 0
    for y in x:
        hidden = softplus400(mulmv400x784(g_fc1)(y) + g_fc1_b)
        z_loc = mulmv50x400(g_fc21)(hidden) + g_fc21_b
        z_scale = exp50(mulmv50x400(g_fc22)(hidden) + g_fc22_b)
        i = i + 1
        pyro.sample("z_" + str(i), dist.Normal(z_loc, z_scale))


cond_hmodel = pyro.condition(hmodel, data=data)


_, gi = timeit(test, cond_hmodel, hguide)
print("hand-written code inference time: %fs" % (gi / 1e9))
