#ifndef KITTEN_H
#define KITTEN_H

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum KType {
  K_UNBOXED = 0x00,
  K_FLOAT = K_UNBOXED,
  K_INT,
  K_NONE,
  K_UNIT,
  K_BOXED = 0x10,
  K_ACTIVATION = K_BOXED,
  K_HANDLE,
  K_LEFT,
  K_PAIR,
  K_RIGHT,
  K_SOME,
  K_VECTOR
} KType;

typedef struct KObject {
  uint64_t data;
  uint64_t type;
} KObject;

typedef struct KActivation {
  uint64_t refs;
  void* function;
  KObject* begin;
  KObject* end;
} KActivation;

typedef struct KHandle {
  uint64_t refs;
  FILE* value;
} KHandle;

typedef struct KPair {
  uint64_t refs;
  KObject first;
  KObject rest;
} KPair;

typedef struct KSole {
  uint64_t refs;
  KObject value;
} KSole;

typedef struct KVector {
  uint64_t refs;
  KObject* begin;
  KObject* end;
  KObject* capacity;
} KVector;

typedef KSole KLeft, KRight, KSome;

typedef union KBoxed {
  uint64_t refs;
  KActivation as_activation;
  KHandle as_handle;
  KLeft as_left;
  KPair as_pair;
  KRight as_right;
  KSome as_some;
  KVector as_vector;
} KBoxed;

typedef struct KR {
  void* address;
  int closure;
} KR;

extern KR* k_return;
extern KObject** k_closure;
extern KObject* k_data;
extern KObject* k_locals;

void k_init(void);

KObject k_retain(KObject object);
KObject k_release(KObject object);

KObject k_activation(void (*function)(void), size_t size, ...);
KObject k_float(double value);
KObject k_handle(FILE* value);
KObject k_int(int64_t value);
KObject k_left(KObject value);
KObject k_none(void);
KObject k_pair(KObject first, KObject rest);
KObject k_right(KObject value);
KObject k_some(KObject value);
KObject k_unit(void);
KObject k_vector(size_t size, ...);

KObject k_append_vector(KObject a, KObject b);
KObject k_make_vector(size_t size);

#define k_drop_data() (--k_data)
#define k_pop_data() (*k_data--)
#define k_push_data(x) (*++k_data = (x))
#define k_drop_locals() (--k_locals)
#define k_pop_locals() (*k_locals--)
#define k_push_locals(x) (*++k_locals = (x))
#define k_get_local(i) (*(k_locals - (i)))
#define k_get_closure(i) ((*k_closure)[(i)])
#define k_pop_return() (*k_return--)

#endif
