#include <stdio.h>

int is_bigendian() {
  int n = 1;
  // little endian if true
  if(*(char *)&n == 1) {
    return 1;
  }
  return 0;
}

int reverseInt_cond (char *c) {
    int i;
    char *p = (char *)&i;

    if (is_bigendian()) {
        p[0] = c[0];
        p[1] = c[1];
        p[2] = c[2];
        p[3] = c[3];
    } else {
        p[0] = c[3];
        p[1] = c[2];
        p[2] = c[1];
        p[3] = c[0];
    }

    return i;
}

int reverseInt (char *c) {
    int i;
    char *p = (char *)&i;
    p[0] = c[3];
    p[1] = c[2];
    p[2] = c[1];
    p[3] = c[0];
    return i;
}

unsigned long long reverseLonglong (char *c) {
    long long i;
    char *p = (char *)&i;
    p[0] = c[7];
    p[1] = c[6];
    p[2] = c[5];
    p[3] = c[4];
    p[4] = c[3];
    p[5] = c[2];
    p[6] = c[1];
    p[7] = c[0];
    return i;
}

double reverseDouble (char *c) {
    double d;
    char *p = (char *)&d;
    p[0] = c[7];
    p[1] = c[6];
    p[2] = c[5];
    p[3] = c[4];
    p[4] = c[3];
    p[5] = c[2];
    p[6] = c[1];
    p[7] = c[0];
    return d;
}

int main () {

  union {
    int i;
    char c[sizeof(int)];
  } x;
  x.i = 1;
  if(x.c[0] == 1)
    printf("little-endian\n");
  else    printf("big-endian\n");
  
  union {
    int i;    
    char c[sizeof(int)];
  } y;

  strcpy(y.c,"ABCD");
  printf("y.c %s\n",y.c);
  printf("y.i %d\n",y.i);
  y.i = reverseInt(y.c);
  printf("reversed y.i %d\n",y.i);
  printf("reversed y.c %s\n",y.c);

  union {
    unsigned long long i;    
    char c[sizeof(long long)];
    double d;
  } z;

  union {
    double d;
    char c[sizeof(double)];
  } w;

  /* strcpy(z.c,"ABCDEFGH"); */
  /* printf("z.c %s\n"  ,z.c); */
  /* printf("z.i %lld\n",z.i); */
  /* printf("z.d %f\n"  ,z.d); */
  /* z.i = reverseLonglong(z.c); */
  /* printf("reversed z.c %s\n"  ,z.c); */
  /* printf("reversed z.i %lld\n",z.i); */
  /* // Cela ne marche pas, le nombre semble trop court. Voir ci-dessous */
  /* printf("reversed z.d %f\n"  ,z.d); */
  
  /* z.i = 4702394921427289928; */
  /* printf("z.c %s\n",z.c); */
  /* z.i = reverseLonglong(z.c); */
  /* printf("z.c %s\n",z.c); */

  /* // Cela ne marche pas, le nombre semble trop court. */
  /* z.d=2393736.541207; */
  /* printf("z.c %s\n",z.c); */

  strcpy(w.c,"ABCDEFGH");
  printf("w.c %s\n"  ,w.c);
  printf("w.d %f\n"  ,w.d);
  w.d = reverseDouble(w.c);
  printf("reversed w.c %s\n"  ,w.c);
  printf("reversed w.d %f\n"  ,w.d);
  w.d = reverseDouble(w.c);
  printf("reversed 2 w.c %s\n"  ,w.c);
  printf("reversed 2 w.d %f\n"  ,w.d);
  

}
