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
