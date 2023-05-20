/*
  eBPF QuickCheck helper library
  --------------------------------
  A small collection of functions wrapping the bpf-syscall,
  designed for writing e.g. Haskell bindings such that
  QuickCheck or similar can be applied to eBPF

  
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <assert.h>
#include <linux/bpf.h>
#include <linux/bpf_common.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <error.h>
#include "bpf_insn.h"
#include "lib_ebpf_qc.h"

//#define DEBUG 1
#ifdef DEBUG
   #define INFO_PRINT printf
#else
   #define INFO_PRINT(...)
#endif



static int bpf(unsigned cmd, union bpf_attr *attr, size_t size)
{
    // 321 == sys_bpf
    // man 2 bpf
    return syscall(321, cmd, attr, size);
}


int create_array_map(uint32_t value_size, uint32_t key_size, uint32_t max_entries) {
    union bpf_attr map = {
	.map_type = BPF_MAP_TYPE_ARRAY,
	.key_size = key_size,
	.value_size = value_size,
	.max_entries = max_entries,
    };
    int map_fd;
    do {
	map_fd = (__u32)bpf(BPF_MAP_CREATE, &map, sizeof(map));
    } while (map_fd < 0 && errno == EAGAIN);

    return map_fd;
}


int create_array_map_with_constant32(uint32_t value_size, uint32_t key_size, uint32_t max_entries, uint32_t constant) {

    // We can only do this if the value size is 4 bytes
    assert(value_size == 4);
    // Create the map
    int map_fd = create_array_map(value_size, key_size, max_entries);
    // Init each 32-bit element as constant
    uint32_t buf[1];
    buf[0] = constant;
    for (int i = 0; i < max_entries; i++) {
	union bpf_attr update_map = {
	    .map_fd = map_fd,
	    .key = (uint64_t)&i,
	    .value = (uint64_t)&buf
	};
	bpf(BPF_MAP_UPDATE_ELEM, &update_map, sizeof(update_map));	
    }
    return map_fd;
}


int create_array_map_with_constant64(uint32_t value_size, uint32_t key_size, uint32_t max_entries, uint64_t constant) {

    // We can only do this if the value size is 8 bytes
    assert(value_size == 8);
    int map_fd = create_array_map(value_size, key_size, max_entries);
    
    // Init each 64-bit element as constant
    uint64_t buf[1];
    buf[0] = constant;
    for (int i = 0; i < max_entries; i++) {
	uint32_t key = (uint32_t)i;
	union bpf_attr update_map = {
	    .map_fd = map_fd,
	    .key = (uint64_t)&key,
	    .value = (uint64_t)&buf
	};
	bpf(BPF_MAP_UPDATE_ELEM, &update_map, sizeof(update_map));
    }
    return map_fd;
}


int create_array_map_with_constant_arb(uint32_t value_size, uint32_t key_size, uint32_t max_entries, uint32_t constant) {
    int map_fd = create_array_map(value_size, key_size, max_entries);
    int elms = value_size / 4;
    // Init each 64-bit element as constant
    uint32_t buf[elms];
    for (int i = 0; i < elms; i++) {
	buf[i] = constant;
    }
    for (int i = 0; i < max_entries; i++) {
	uint32_t key = (uint32_t)i;
	union bpf_attr update_map = {
	    .map_fd = map_fd,
	    .key = (uint64_t)&key,
	    .value = (uint64_t)&buf
	};
	bpf(BPF_MAP_UPDATE_ELEM, &update_map, sizeof(update_map));
    }
    return map_fd;
}

int update_array_map32(int map_fd, uint32_t key, uint32_t value) {
    uint32_t buf[1];
    buf[0] = value;
    union bpf_attr update_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&buf,
    };
    int ret_val;
    if (ret_val = bpf(BPF_MAP_UPDATE_ELEM, &update_map, sizeof(update_map)) < 0) {
	perror("Error in update_array_map32");
    }
    return ret_val;
}


int update_array_map64(int map_fd, uint32_t key, uint64_t value) {
    uint64_t buf[1];
    buf[0] = value;
    union bpf_attr update_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&buf,
    };
    int ret_val;
    if (ret_val = bpf(BPF_MAP_UPDATE_ELEM, &update_map, sizeof(update_map)) < 0) {
	perror("Error in update_array_map64");
    }
    return ret_val;
}

// hacky hacky. This should really have some sort of union type as return,
// so we can distinguish between failed and successful reads
uint32_t read_array_map32(int map_fd, uint32_t key) {
    uint32_t ret_val;
    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&ret_val
    };

    bpf(BPF_MAP_LOOKUP_ELEM, &lookup_map, sizeof(lookup_map));
    return ret_val;
}

// hacky hacky. This should really have some sort of union type as return,
// so we can distinguish between failed and successful reads
uint64_t read_array_map64(int map_fd, uint32_t key) {
    uint64_t ret_val;
    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&ret_val
    };

    bpf(BPF_MAP_LOOKUP_ELEM, &lookup_map, sizeof(lookup_map));
    return ret_val;
}


// Arbitrary read of 8 bytes "somewhere in a map" where value_size > 8
uint64_t read_array_map_arb(int map_fd, uint64_t key, uint32_t offset, uint32_t size) {
    uint64_t ret_val;
    /* uint64_t key = 0; */
    uint64_t buf[size / sizeof(uint64_t)];
    memset(buf, 0, sizeof(buf));
    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&buf
    };

    if (bpf(BPF_MAP_LOOKUP_ELEM, &lookup_map, sizeof(lookup_map)) < 0) {
	printf("Oh damn, lookup failed..\n");
    }
    ret_val = buf[offset];
    return ret_val;
}

// Arbitrary read of 4 bytes "somewhere in a map" where value_size > 4
uint32_t read_array_map_arb32(int map_fd, uint64_t key, uint32_t offset, uint32_t size) {
    uint32_t ret_val;
    /* uint64_t key = 0; */
    uint32_t buf[size / sizeof(uint32_t)];
    memset(buf, 0, sizeof(buf));
    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&buf
    };

    if (bpf(BPF_MAP_LOOKUP_ELEM, &lookup_map, sizeof(lookup_map)) < 0) {
	printf("Oh damn, lookup failed..\n");
    }
    ret_val = buf[offset];
    return ret_val;
}


// Arbitrary write of 8 bytes "somewhere in a map" where value_size > 8
// Overwrites all other locations
uint64_t write_array_map_arb(int map_fd, uint64_t key, uint32_t offset, uint32_t size, uint64_t value) {
    // First we need to grab the full existing value store at location "key"
    uint64_t buf[size / sizeof(uint64_t)];
    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&buf
    };

    if (bpf(BPF_MAP_LOOKUP_ELEM, &lookup_map, sizeof(lookup_map)) < 0) {
	printf("Oh damn, lookup failed..\n");
    }

    buf[offset] = value;
    union bpf_attr update_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&buf
    };

    if (bpf(BPF_MAP_UPDATE_ELEM, &update_map, sizeof(update_map)) < 0) {
	printf("Oh damn, update failed..\n");
	return 1;
    }
    return 0;
}


/*  Stack map helper functions */ 


int create_stack_map(uint32_t value_size, uint32_t max_entries) {
    union bpf_attr map = {
	.map_type = BPF_MAP_TYPE_STACK,
	// key_size has to be 0 in stack maps
	.key_size = 0,
	.value_size = value_size,
	.max_entries = max_entries
    };

    int map_fd = (__u32)bpf(BPF_MAP_CREATE, &map, sizeof(map));
    if (map_fd < 0) {
	perror("Error in create_stack_map");
    }
    return map_fd;
}

int stack_map_push64(int map_fd, uint64_t value) {
    uint64_t buf[1];
    buf[0] = value;
    
    union bpf_attr update_map = {
	.map_fd = map_fd,
	.key = (uint64_t)NULL,
	.value = (uint64_t)&buf
    };
    if (bpf(BPF_MAP_UPDATE_ELEM, &update_map, sizeof(update_map)) < 0) {
	return -1;
    }
    return 0;
}

int stack_map_push32(int map_fd, uint32_t value) {
    uint32_t buf[1];
    buf[0] = value;
    
    union bpf_attr update_map = {
	.map_fd = map_fd,
	.key = (uint64_t)NULL,
	.value = (uint64_t)&buf
    };
    return bpf(BPF_MAP_UPDATE_ELEM, &update_map, sizeof(update_map));
}

uint64_t stack_map_pop64(int map_fd) {
    uint64_t buf[1];

    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)NULL,
	.value = (uint64_t)&buf
    };
    if (bpf(BPF_MAP_LOOKUP_AND_DELETE_ELEM, &lookup_map, sizeof(lookup_map)) < 0) {
	perror("Error in stack pop64!");
	return 42;
    }
    return buf[0];
}

uint32_t stack_map_pop32(int map_fd){
    uint32_t buf[1];

    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)NULL,
	.value = (uint64_t)&buf
    };
    if (bpf(BPF_MAP_LOOKUP_AND_DELETE_ELEM, &lookup_map, sizeof(lookup_map)) < 0) {
	perror("Error in stack pop32!");
    }
    return buf[0];
}




/*
  Attempt to load the eBPF-program supplied as `*instructions`
  as a SOCKET_FILTER program.
  Loading a program means running it through the verifier and,
  if successful, receiving af file descriptor referring to the loaded eBPF program.
 */
int load_prog(char *prog_bytes, size_t insn_count, bool verbose)
{
    struct bpf_insn *instructions = (struct bpf_insn*)prog_bytes;
    uint32_t LOG_SIZE = 0;
    char *logbuf = NULL;
    int LOG_LEVEL = 0;
    // Set up a buffer for logging
    if (verbose) {
	LOG_SIZE = 65536;
	logbuf = malloc(LOG_SIZE);
	LOG_LEVEL = 3;
    }
    union bpf_attr prog = {
	    .prog_type = BPF_PROG_TYPE_SOCKET_FILTER,
	    .insns = (uint64_t)instructions,
	    .insn_cnt = insn_count,
	    .license = (uint64_t)"GPL",
	    // Logging
	    .log_level = LOG_LEVEL,
	    .log_size = LOG_SIZE,
	    .log_buf = (__aligned_u64)logbuf
	};
    // load the BPF program
    int prog_fd;
    do {
	prog_fd = bpf(BPF_PROG_LOAD, &prog, sizeof(prog));	    
    } while (prog_fd < 0 && errno == EAGAIN);

    if (prog_fd < 0 && verbose) {
	int errsv = errno;
	printf("[-] Load of bpf prog failed.\nerr: %d (errno = %d)\n", prog_fd, errsv);
	perror("Error in load_prog");
	printf("%s\n", logbuf);
    }
    if (verbose) {
	free(logbuf);
    }
    return prog_fd;
}

/*
  Trigger execution of an already loaded eBPF program referred to by
  the file descriptor `prog_fd`.

  Triggering happens by
  - Creating a socket pair
  - Attaching the program to one of the sockets
  - Writing to that socket
*/
int trigger_prog(int prog_fd) {
  int sockets[2];
  int res = 1;

  if ( 0 != socketpair(AF_UNIX, SOCK_DGRAM , 0, sockets )) {
    printf("[-] Setup of socket pair to trigger program failed.\n");
    perror("Error in trigger_prog");
    return 0;
  }

  if ( 0 > setsockopt(sockets[0], SOL_SOCKET, SO_ATTACH_BPF, &prog_fd, sizeof(prog_fd)) ) {
    printf("[-] Failed to attach program to socket.\n");
    perror("Error in trigger_prog");
    res = 0;
    goto DONE;
  }

  // trigger execution by writing a dummy message through the sockets.
  if ( 1 != write(sockets[1], "B", 1) ) {
    printf("[-] Failed to write to socket.\n");
    perror("Error in trigger_prog");
    res = 0;
    goto DONE;
  }

  // The following read will block if the filter returns 0
  /* char dummy; */
  /* if ( 1 != read(sockets[0], &dummy, 1) ) { */
  /*   printf("[-] Failed to read from socket.\n"); */
  /*   perror("Error in trigger_prog"); */
  /*   close(sockets[0]); */
  /*   close(sockets[1]); */
  /*   return 0; */
  /* } */

 DONE:
  close(sockets[0]);
  close(sockets[1]);
  return res;
}

/*
  Attempt to run an eBPF program.
  map_fd refers to a file descriptor referencing an eBPF array map.
  The program is expected to write a value to the map with key=0.
  After execution, the value is read as an unsigned 32-bit integer
  and returned.
 */
uint32_t run_bpf(int map_fd, char *prog_bytes, int insn_count, bool verbose) {
    int prog_fd = load_prog(prog_bytes, insn_count, verbose);
    if (prog_fd < 0) {
	exit(1);
    }

    // Trigger the program
    int nouse = trigger_prog(prog_fd);
        
    // Read value from map
    int key = 0;
    uint32_t ret_val;

    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&ret_val
    };

    assert(bpf(BPF_MAP_LOOKUP_ELEM, &lookup_map, sizeof(lookup_map)) == 0);

    close(map_fd);
    close(prog_fd);
    return ret_val;
}

/*
  Attempt to run an eBPF program. 
  map_fd refers to a file descriptor referencing an eBPF array map. 
  The program is expected to write a value to the map with key=0. 
  After execution, the value is read as an unsigned 64-bit integer 
  and returned.
*/
uint64_t run_bpf64(int map_fd, char *prog_bytes, int insn_count, bool verbose) {
    int prog_fd = load_prog(prog_bytes, insn_count, verbose);
    if (prog_fd < 0) {
	exit(1);
    }

    // Trigger the program
    int nouse = trigger_prog(prog_fd);
    // Read value from map
    int key = 0;
    uint64_t ret_val;

    union bpf_attr lookup_map = {
	.map_fd = map_fd,
	.key = (uint64_t)&key,
	.value = (uint64_t)&ret_val
    };

    assert(bpf(BPF_MAP_LOOKUP_ELEM, &lookup_map, sizeof(lookup_map)) == 0);

    close(map_fd);
    close(prog_fd);
    return ret_val;
}

int close_fd(int fd) { return close(fd); }
