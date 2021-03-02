def hmodel():
    weight = pyro.sample("weight", dist.Normal(8.5, 1.))
    return pyro.sample("measurement", dist.Normal(weight, 0.75))
def hguide():
    a = pyro.param("a", torch.tensor(8.5))
    b = pyro.param("b", torch.tensor(1.), constraint=constraints.positive)
    return pyro.sample("weight", dist.Normal(a, b))
