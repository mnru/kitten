#ifndef KITTEN_H
#define KITTEN_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

enum KittenType {
  KITTEN_ACTIVATION,
  KITTEN_FLOAT,
  KITTEN_HANDLE,
  KITTEN_INT,
  KITTEN_LEFT,
  KITTEN_PAIR,
  KITTEN_RIGHT,
  KITTEN_SOME,
  KITTEN_UNIT,
  KITTEN_VECTOR
};

union KittenObject;

typedef struct KittenActivation {
  uint16_t type;
  uint16_t refcount;
  void (*function)(void);
  union KittenObject** begin;
  union KittenObject** end;
} KittenActivation;

typedef struct KittenBox {
  uint16_t type;
  uint16_t refcount;
  union KittenObject* value;
} KittenBox;

typedef struct KittenFloat {
  uint16_t type;
  uint16_t refcount;
  double value;
} KittenFloat;

typedef struct KittenHandle {
  uint16_t type;
  uint16_t refcount;
  FILE* value;
} KittenHandle;

typedef struct KittenInt {
  uint16_t type;
  uint16_t refcount;
  int64_t value;
} KittenInt;

typedef struct KittenPair {
  uint16_t type;
  uint16_t refcount;
  union KittenObject* first;
  union KittenObject* rest;
} KittenPair;

typedef struct KittenUnit {
  uint16_t type;
  uint16_t refcount;
} KittenUnit;

typedef struct KittenVector {
  uint16_t type;
  uint16_t refcount;
  union KittenObject** begin;
  union KittenObject** end;
  union KittenObject** capacity;
} KittenVector;

typedef union KittenObject {
  uint16_t type;
  uint16_t refcount;
  KittenActivation as_activation;
  KittenBox as_box;
  KittenFloat as_float;
  KittenHandle as_handle;
  KittenInt as_int;
  KittenPair as_pair;
  KittenUnit as_unit;
  KittenVector as_vector;
} KittenObject;

extern void** kitten_call;
extern KittenObject** kitten_closure;
extern KittenObject** kitten_data;
extern KittenObject** kitten_locals;

#define KITTEN_INT_POOL_SIZE 128

extern KittenObject* kitten_ints[KITTEN_INT_POOL_SIZE];
extern KittenObject* kitten_unit;

void kitten_init(void);

KittenObject* kitten_retain(KittenObject* object);
KittenObject* kitten_release(KittenObject* object);

KittenObject* kitten_new_activation(void (*function)(void), size_t size, ...);
KittenObject* kitten_new_float(double value);
KittenObject* kitten_new_handle(FILE* value);
KittenObject* kitten_new_int(int64_t value);
KittenObject* kitten_new_left(KittenObject* value);
KittenObject* kitten_new_pair(KittenObject* first, KittenObject* rest);
KittenObject* kitten_new_right(KittenObject* value);
KittenObject* kitten_new_some(KittenObject* value);
KittenObject* kitten_new_unit();
KittenObject* kitten_new_vector(size_t size, ...);

#endif
