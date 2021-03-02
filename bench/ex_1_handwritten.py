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
