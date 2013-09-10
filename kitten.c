#include "kitten.h"

#include <stdarg.h>
#include <stdlib.h>

KR* k_return;
KObject** k_closure;
KObject* k_data;
KObject* k_locals;

void k_init(void) {
  const size_t CLOSURE_SIZE = 1024;
  k_closure = calloc(CLOSURE_SIZE, sizeof(KObject*));

  const size_t RETURN_SIZE = 1024;
  k_return = calloc(RETURN_SIZE, sizeof(KR));

  const size_t DATA_SIZE = 1024;
  k_data = calloc(DATA_SIZE, sizeof(KObject));

  const size_t LOCALS_SIZE = 1024;
  k_locals = calloc(LOCALS_SIZE, sizeof(KObject));
}

static int is_boxed_type(const KType type) {
  return type >= K_BOXED;
}

static uint64_t* boxed_refs(KObject* const object) {
  return &(*((KBoxed**)&object->data))->refs;
}

KObject k_retain(const KObject object) {
  if (is_boxed_type(object.type))
    ++(*((KBoxed**)&object.data))->refs;
  return object;
}

KObject k_release(KObject object) {

  if (!is_boxed_type(object.type))
    return object;

  if (--*boxed_refs(&object) > 0)
    return object;

  // TODO Free members.

  free((KObject*)object.data);
  return object;

}

KObject k_activation(void (*const function)(void), const size_t size, ...) {
  KActivation* const activation = calloc(1, sizeof(KActivation));
  activation->refs = 1;
  activation->function = function;
  activation->begin = calloc(size, sizeof(KObject));
  activation->end = activation->begin + size;
  va_list args;
  va_start(args, size);
  for (size_t i = 0; i < size; ++i) {
    activation->begin[i] = k_retain(va_arg(args, KObject));
  }
  return (KObject) {
    .data = (uint64_t)activation,
    .type = K_ACTIVATION
  };
}

KObject k_bool(const k_bool_t value) {
  return (KObject) {
    .data = !!value,
    .type = K_BOOL
  };
}

KObject k_char(const k_char_t value) {
  return (KObject) {
    .data = value,
    .type = K_CHAR
  };
}

KObject k_float(const k_float_t value) {
  return (KObject) {
    .data = *((uint64_t*)&value),
    .type = K_FLOAT
  };
}

KObject k_handle(const k_handle_t value) {
  return (KObject) {
    .data = *((uint64_t*)&value),
    .type = K_FLOAT
  };
}

KObject k_int(const k_int_t value) {
  return (KObject) {
    .data = value,
    .type = K_INT
  };
}

static KObject k_sole(const KObject value) {
  const KObject result = {
    .data = (uint64_t)calloc(1, sizeof(KObject)),
    .type = K_UNIT  // FIXME?
  };
  *((KObject*)result.data) = k_retain(value);
  return result;
}

KObject k_left(const KObject value) {
  KObject result = k_sole(value);
  result.type = K_LEFT;
  return result;
}

KObject k_none() {
  return (KObject) {
    .data = 0,
    .type = K_NONE
  };
}

KObject k_pair(const KObject first, const KObject rest) {
  KPair* pair = calloc(1, sizeof(KPair));
  pair->first = k_retain(first);
  pair->rest = k_retain(rest);
  return (KObject) {
    .data = (uint64_t)pair,
    .type = K_PAIR
  };
}

KObject k_right(const KObject value) {
  KObject result = k_sole(value);
  result.type = K_LEFT;
  return result;
}

KObject k_some(const KObject value) {
  KObject result = k_sole(value);
  result.type = K_SOME;
  return result;
}

KObject k_unit() {
  return (KObject) {
    .data = 0,
    .type = K_UNIT
  };
}

KObject k_append_vector(
  const KObject a, const KObject b) {
  KVector* vector = calloc(1, sizeof(KVector));
  const size_t size
    = ((KVector*)a.data)->end - ((KVector*)a.data)->begin
    + ((KVector*)b.data)->end - ((KVector*)b.data)->begin;
  vector->begin = calloc(size, sizeof(KObject));
  vector->capacity = vector->begin + size;
  vector->end = vector->begin;
  for (KObject* from = ((KVector*)a.data)->begin;
       from != ((KVector*)a.data)->end; ++from) {
    *vector->end++ = k_retain(*from);
  }
  for (KObject* from = ((KVector*)b.data)->begin;
       from != ((KVector*)b.data)->end; ++from) {
    *vector->end++ = k_retain(*from);
  }
  return (KObject) {
    .data = (uint64_t)vector,
    .type = K_VECTOR
  };
}

KObject k_vector(const size_t size, ...) {
  va_list args;
  va_start(args, size);
  KVector* vector = calloc(1, sizeof(KVector));
  vector->begin = calloc(size, sizeof(KObject));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  for (size_t i = 0; i < size; ++i) {
    vector->begin[i] = k_retain(va_arg(args, KObject));
  }
  va_end(args);
  return (KObject) {
    .data = (uint64_t)vector,
    .type = K_VECTOR
  };
}

KObject k_make_vector(const size_t size) {
  KVector* vector = calloc(1, sizeof(KVector));
  vector->begin = calloc(size, sizeof(KObject*));
  vector->end = vector->begin + size;
  vector->capacity = vector->begin + size;
  for (size_t i = 0; i < size; ++i) {
    vector->begin[i] = k_data[size - 1 - i];
  }
  return (KObject) {
    .data = (uint64_t)vector,
    .type = K_VECTOR
  };
}
