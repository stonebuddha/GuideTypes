#ifndef __TORCH_EXT_H__
#define __TORCH_EXT_H__

#ifdef __cplusplus
extern "C" {
typedef torch::Tensor *tensor;
#define PROTECT(x) \
  try { \
    x \
  } catch (const exception& e) { \
    caml_failwith(strdup(e.what())); \
  }
#else
typedef void *tensor;
#endif

tensor at_bool_vec(bool *values, int value_len, int type);
bool at_bool_value_at_indexes(tensor, int *indexes, int indexes_len);

tensor at_dirichlet(tensor concentration);

#ifdef __cplusplus
};
#endif

#endif
