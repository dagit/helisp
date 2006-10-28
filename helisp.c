/* a simple driver for helisp */
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define fixnum_mask  3
#define fixnum_tag   0
#define fixnum_shift 2
#define char_mask    255
#define char_tag     15
#define char_shift   8
#define bool_mask    127
#define bool_tag     31
#define bool_shift   7
#define empty_list   47

extern long helisp_entry(char* p);

static char* allocate_protected_space(int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if ( p == MAP_FAILED) { exit(1); }
  status = mprotect(p, page, PROT_NONE);
  if ( status != 0 ) { exit(1); }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if ( status != 0 ) { exit(1); }
  return ( p + page );
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if( status != 0 ){ exit(1); }
}

void print_ptr(long val){
  if ( val == empty_list ){
    printf("()");
  } else if( (val & fixnum_mask) == fixnum_tag ){
    printf("%ld", val >> fixnum_shift);
  } else if( (val & char_mask) == char_tag ){
    printf("%c", (int)(val >> char_shift));
  } else if( (val & bool_mask) == bool_tag ){
    if ( (val >> bool_shift) == 1 ){
      printf("#t");
    } else if( (val >> bool_shift) == 0 ){
      printf("#f");
    } else {
      printf("bad bool: %d", val >> bool_shift);
    }
  } else {
    printf("unkown type for value: %ld", val );
  }
}

int main(int argc, char** argv){
  int stack_size = (16 * 4096); /* holds 16K cells */
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  print_ptr(helisp_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  return 0;

}
