#include "kitten.h"

#include <stdarg.h>
#include <stdlib.h>

KittenObject** kitten_closure;
KittenObject** kitten_data;
KittenObject** kitten_locals;

KittenObject* kitten_ints[KITTEN_INT_POOL_SIZE];
KittenObject* kitten_unit;

void kitten_init(void) {
  const size_t DATA_SIZE = 1024;
  kitten_data = calloc(DATA_SIZE, sizeof(KittenObject*));
  kitten_data += DATA_SIZE - 1;

  const size_t LOCALS_SIZE = 1024;
  kitten_locals = calloc(LOCALS_SIZE, sizeof(KittenObject*));
  kitten_locals += LOCALS_SIZE - 1;

  for (int i = 0; i < KITTEN_INT_POOL_SIZE; ++i) {
    kitten_ints[i] = kitten_new_int(i);
  }

  kitten_unit = kitten_new_unit();
}

KittenObject* kitten_retain(KittenObject* const object) {
  ++object->refcount;
  return object;
}

KittenObject* kitten_release(KittenObject* const object) {
  if (--object->refcount > 0)
    return object;
  switch (object->type) {
  case KITTEN_ACTIVATION:
    for (KittenObject** p = object->as_activation.begin;
         p != object->as_activation.end; ++p) {
      kitten_release(*p);
    }
    break;
  case KITTEN_FLOAT:
    break;
  case KITTEN_HANDLE:
    fclose(object->as_handle.value);
    break;
  case KITTEN_INT:
    break;
  case KITTEN_LEFT:
  case KITTEN_RIGHT:
  case KITTEN_SOME:
    kitten_release(object->as_box.value);
    break;
  case KITTEN_PAIR:
    kitten_release(object->as_pair.first);
    kitten_release(object->as_pair.rest);
    break;
  case KITTEN_UNIT:
    break;
  case KITTEN_VECTOR:
    for (KittenObject** p = object->as_vector.begin;
         p != object->as_vector.end; ++p) {
      kitten_release(*p);
    }
    break;
  }
  free(object);
  return NULL;
}

void* kitten_alloc(const size_t size, const uint16_t type) {
  KittenObject* object = malloc(size);
  object->type = type;
  object->refcount = 0;
  return object;
}

KittenObject* kitten_new_activation(
  void (*const function)(void), const size_t size, ...) {
  KittenActivation* object = kitten_alloc(
    sizeof(KittenActivation), KITTEN_ACTIVATION);
  object->function = function;
  object->begin = calloc(size, sizeof(KittenObject*));
  object->end = object->begin + size;
  va_list args;
  va_start(args, size);
  for (size_t i = 0; i < size; ++i) {
    object->begin[i] = kitten_retain(va_arg(args, KittenObject*));
  }
  return (KittenObject*)object;
}

KittenObject* kitten_new_float(const double value) {
  KittenFloat* object = kitten_alloc(sizeof(KittenFloat), KITTEN_FLOAT);
  object->value = value;
  return (KittenObject*)object;
}

KittenObject* kitten_new_handle(FILE* const value) {
  KittenHandle* object = kitten_alloc(sizeof(KittenHandle), KITTEN_HANDLE);
  object->value = value;
  return (KittenObject*)object;
}

KittenObject* kitten_new_int(const int64_t value) {
  KittenInt* object = kitten_alloc(sizeof(KittenInt), KITTEN_INT);
  object->value = value;
  return (KittenObject*)object;
}

KittenObject* kitten_new_left(KittenObject* const value) {
  KittenBox* object = kitten_alloc(sizeof(KittenBox), KITTEN_LEFT);
  object->value = value;
  return (KittenObject*)object;
}

KittenObject* kitten_new_pair(
  KittenObject* const first,
  KittenObject* const rest) {
  KittenPair* object = kitten_alloc(sizeof(KittenPair), KITTEN_PAIR);
  object->first = first;
  object->rest = rest;
  return (KittenObject*)object;
}

KittenObject* kitten_new_right(KittenObject* const value) {
  KittenBox* object = kitten_alloc(sizeof(KittenBox), KITTEN_RIGHT);
  object->value = value;
  return (KittenObject*)object;
}

KittenObject* kitten_new_some(KittenObject* const value) {
  KittenBox* object = kitten_alloc(sizeof(KittenBox), KITTEN_SOME);
  object->value = value;
  return (KittenObject*)object;
}

KittenObject* kitten_new_unit(KittenObject* const value) {
  KittenBox* object = kitten_alloc(sizeof(KittenUnit), KITTEN_UNIT);
  return (KittenObject*)object;
}

KittenObject* kitten_new_vector(const size_t size, ...) {
  va_list args;
  va_start(args, size);
  KittenVector* object = kitten_alloc(sizeof(KittenVector), KITTEN_VECTOR);
  object->begin = calloc(size, sizeof(KittenObject*));
  object->end = object->begin + size;
  object->capacity = object->begin + size;
  for (size_t i = 0; i < size; ++i) {
    object->begin[i] = kitten_retain(va_arg(args, KittenObject*));
  }
  va_end(args);
  return (KittenObject*)object;
}
