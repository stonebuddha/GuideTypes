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
