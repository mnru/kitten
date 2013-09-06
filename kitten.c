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
  KActivation* const object = calloc(1, sizeof(KActivation));
  object->refs = 1;
  object->function = function;
  object->begin = calloc(size, sizeof(KObject));
  object->end = object->begin + size;
  va_list args;
  va_start(args, size);
  for (size_t i = 0; i < size; ++i) {
    object->begin[i] = k_retain(va_arg(args, KObject));
  }
  return (KObject) {
    .data = *((uint64_t*)&object),
    .type = K_ACTIVATION,
  };
}

KObject k_float(const double value) {
  return (KObject) {
    .data = *((uint64_t*)&value),
    .type = K_FLOAT,
  };
}

KObject k_handle(FILE* const value) {
  return (KObject) {
    .data = *((uint64_t*)&value),
    .type = K_FLOAT,
  };
}

KObject k_int(const int64_t value) {
  return (KObject) {
    .data = *((uint64_t*)&value),
    .type = K_FLOAT,
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
    .type = K_NONE,
  };
}

KObject k_pair(const KObject first, const KObject rest) {
  KPair* pair = calloc(1, sizeof(KPair));
  pair->first = k_retain(first);
  pair->rest = k_retain(rest);
  return (KObject) {
    .data = (uint64_t)pair,
    .type = K_PAIR,
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
    .type = K_UNIT,
  };
}

/*
KObject* k_append_vector(
  KObject* const a, KObject* const b) {
  KVector* object = k_alloc(sizeof(KVector), K_VECTOR);
  const size_t size = a->as_vector.end - a->as_vector.begin
    + b->as_vector.end - b->as_vector.begin;
  object->begin = calloc(size, sizeof(KObject*));
  object->capacity = object->begin + size;
  object->end = object->begin;
  for (KObject** from = a->as_vector.begin;
       from != a->as_vector.end; ++from) {
    *object->end++ = k_retain(*from);
  }
  for (KObject** from = b->as_vector.begin;
       from != b->as_vector.end; ++from) {
    *object->end++ = k_retain(*from);
  }
  return (KObject*)object;
}
*/

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
