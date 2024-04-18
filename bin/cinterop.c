#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void ll_puts(int8_t *s) {
  puts((char *)s);
}

int8_t* ll_strcat(int8_t* s1, int8_t* s2) {
  int l1 = strlen((char*)s1);
  int l2 = strlen((char*)s2);
  char* buf = (char*)calloc(l1 + l2 + 1, sizeof(char));
  strncpy(buf, (char*)s1, l1);
  strncpy(buf + l1, (char*)s2, l2+1);
  return (int8_t*) buf;
}

int64_t ll_callback(int64_t (*fun)(int64_t, int64_t)) {
  int64_t x = 19L;
  return fun(x, x);
}

int8_t* ll_ltoa(int64_t i) {
  // Safety: INT64_MIN is -9223372036854775808, which has 20 characters when
  // represented as a string. After including the null terminator, we need to
  // allocate a buffer of size 21.
  char* buf = (char*)calloc(21, sizeof(char));
  int t = 0;
  if (i == 0) {
    buf[t++] = '0';
    return (int8_t*) buf;
  }

  bool negative = i < 0;
  if (!negative) {
    // Normalize to negative number to avoid overflow
    i = -i;
  }

  // Generate the digits in reverse order, from [0..t)
  while (i < 0) {
    char last_digit = '0' + -(i % 10);
    buf[t++] = last_digit;
    i /= 10;
  }
  if (negative) {
    buf[t++] = '-';
  }

  // Reverse the buffer
  for (int j = 0, r = t - 1; j < r; j++, r--) {
    char temp = buf[j];
    buf[j] = buf[r];
    buf[r] = temp;
  }

  return (int8_t*) buf;
}

void *ll_malloc(int64_t n, int64_t size) {
  return calloc(n, size);
}
