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
