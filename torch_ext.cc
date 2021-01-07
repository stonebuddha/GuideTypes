#include<torch/script.h>
#include<caml/fail.h>
#include "torch_ext.h"

using namespace std;

tensor at_bool_vec(bool *vs, int len, int type) {
  PROTECT(
    torch::Tensor tensor = torch::empty({len}, torch::ScalarType(type));
    for (int i = 0; i < len; ++i) tensor[i] = vs[i];
    return new torch::Tensor(tensor);
  )
  return nullptr;
}

bool at_bool_value_at_indexes(tensor t, int *indexes, int indexes_len) {
  PROTECT(
    torch::Tensor tensor = *t;
    for (int i = 0; i < indexes_len; ++i) {
      tensor = tensor[indexes[i]];
    }
    return tensor.item<bool>();
  )
  return bool();
}

using torch::autograd::Function;
using torch::autograd::Variable;
using torch::autograd::AutogradContext;
using torch::autograd::variable_list;

struct Dirichlet : public Function<Dirichlet> {
  static variable_list forward(AutogradContext *ctx, Variable concentration) {
    auto x = at::_sample_dirichlet(concentration);
    ctx->saved_data["x"] = x;
    ctx->saved_data["conc"] = concentration;
    return {x};
  }

  static variable_list backward(AutogradContext *ctx, variable_list grad_output) {
    auto x = ctx->saved_data["x"].toTensor();
    auto concentration = ctx->saved_data["conc"].toTensor();
    auto total = concentration.sum({-1}, true).expand_as(concentration);
    auto grad = at::_dirichlet_grad(x, concentration, grad_output[0]);
    return {grad * (grad_output[0] - (x * grad_output[0]).sum({-1}, true))};
  }
};

tensor at_dirichlet(tensor concentration) {
  PROTECT(
    auto t = *concentration;
    auto r = Dirichlet::apply(t)[0];
    return new torch::Tensor(r);
  )
  return nullptr;
}
