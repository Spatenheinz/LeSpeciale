/* Array map helper function */
int create_array_map(uint32_t value_size, uint32_t key_size, uint32_t max_entries);
int create_array_map_with_constant32(uint32_t value_size, uint32_t key_size, uint32_t max_entries, uint32_t constant);
int create_array_map_with_constant64(uint32_t value_size, uint32_t key_size, uint32_t max_entries, uint64_t constant);
int create_array_map_with_constant_arb(uint32_t value_size, uint32_t key_size, uint32_t max_entries, uint32_t constant);
int update_array_map32(int map_fd, uint32_t key, uint32_t value);
int update_array_map64(int map_fd, uint32_t key, uint64_t value);
uint32_t read_array_map32(int map_fd, uint32_t key);
uint64_t read_array_map64(int map_fd, uint32_t key);
uint64_t read_array_map_arb(int map_fd, uint64_t key, uint32_t offset, uint32_t size);
uint32_t read_array_map_arb32(int map_fd, uint64_t key, uint32_t offset, uint32_t size);
uint64_t write_array_map_arb(int map_fd, uint64_t key, uint32_t offset, uint32_t size, uint64_t value);

/* Stack map helper functions */
int create_stack_map(uint32_t value_size, uint32_t max_entries);
int stack_map_push64(int map_fd, uint64_t value);
int stack_map_push32(int map_fd, uint32_t value);
uint64_t stack_map_pop64(int map_fd);
uint32_t stack_map_pop32(int map_fd);


/* General utility functions */ 
int load_prog(char *prog_bytes, size_t insn_count, bool verbose);
int trigger_prog(int prog_fd);
uint32_t run_bpf(int map_fd, char *prog_bytes, int insn_count, bool verbose);
uint64_t run_bpf64(int map_fd, char *prog_bytes, int insn_count, bool verbose);
int close_fd(int fd);
