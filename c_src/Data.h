/*
 * Data buffer managment
 *
 */
#include <sys/types.h>
#include <machine/endian.h>

typedef struct _data
{
    u_int8_t  dynamic;   /* malloced when dynamic==1 */
    u_int8_t* base;       /* base pointer */
    u_int8_t* ptr;        /* current insertion point */
    u_int8_t* ptr_end;    /* end of buffer (+1) */
    u_int8_t  buf[1];     /* used in some dynamic cases */
} Data;

#define BOOLEAN        0  /* u_int8_t */
#define UINT8          1  /* u_int8_t */
#define UINT16         2  /* u_int16_t */
#define UINT32         3  /* u_int32_t */
#define UINT64         4  /* u_int64_t */
#define STRING1        5  /* len byte followed by UTF-8 chars */
#define LIST           6  /* list begin */
#define LIST_END       7  /* list end */
#define TUPLE          8  /* tuple begin */
#define TUPLE_END      9 /* tuple end */
#define ATOM           10 /* len bytes followed by ASCII chars */
#define BINARY         11 /* binary 4-byte len followed by Octets */
#define INT8           12
#define INT16          13
#define INT32          14
#define INT64          15
#define FLOAT32        16
#define FLOAT64        17
#define STRING4        18 /* 4-byte len followed by UTF-8 string  */

#define PUT_UINT8(ptr, n) do { \
	((u_int8_t*)(ptr))[0] = ((n) & 0xff); \
    } while(0)

#define GET_UINT8(ptr) \
    (((u_int8_t*)(ptr))[0])

#define PUT_UINT16(ptr, n) do { \
	((u_int8_t*)(ptr))[0] = (((n) >> 8) & 0xff);	\
	((u_int8_t*)(ptr))[1] = ((n) & 0xff);		\
    } while(0)

#define GET_UINT16(ptr) \
    ((((u_int8_t*)(ptr))[0] << 8) + ((u_int8_t*)(ptr))[1])

#define PUT_UINT32(ptr, n) do { \
	((u_int8_t*)(ptr))[0] = (((n) >> 24) & 0xff);		\
	((u_int8_t*)(ptr))[1] = (((n) >> 16) & 0xff);		\
	((u_int8_t*)(ptr))[2] = (((n) >> 8) & 0xff);		\
	((u_int8_t*)(ptr))[3] = ((n) & 0xff);			\
    } while(0)

#define GET_UINT32(ptr) \
    ((((u_int8_t*)(ptr))[0] << 24) + (((u_int8_t*)(ptr))[1] << 16) +	\
     (((u_int8_t*)(ptr))[2] << 8) + ((u_int8_t*)(ptr))[3])

#define GET_UINT64(ptr) \
    ((((u_int64_t)GET_UINT32(ptr)) << 32) | \
     GET_UINT32(ptr+4))

#define PUT_UINT64(ptr, n) do { \
	union { u_int64_t u64; u_int32_t u32[2]; } iu; \
	u_int32_t n32;                                 \
	iu.u64 = (n);                                  \
	n32=iu.u32[_QUAD_HIGHWORD]; PUT_UINT32((ptr), n32);  \
	n32=iu.u32[_QUAD_LOWWORD]; PUT_UINT32((ptr)+4, n32); \
    } while(0)

#define PUT_FLOAT32(ptr, n) do { \
	union { float f32; u_int32_t u32; } fu;   \
	u_int32_t n32;                            \
	fu.f32 = (n);                             \
	n32=fu.f32; PUT_UINT32((ptr), n32);       \
    } while(0)

#define PUT_FLOAT64(ptr, n) do { \
	union { double f64; u_int32_t u32[2]; } fu;       \
	u_int32_t n32;                                     \
	fu.f64 = (n);                                   \
	n32=fu.u32[_QUAD_HIGHWORD]; PUT_UINT32((ptr), n32);  \
	n32=fu.u32[_QUAD_LOWWORD]; PUT_UINT32((ptr)+4, n32); \
    } while(0)

static void data_reset(Data* data, u_int32_t skip)
{
    data->ptr = data->base + skip;
}

static void data_init(Data* data, u_int8_t* buf, u_int32_t len, 
		      u_int32_t skip, int dynamic)
{
    data->dynamic = dynamic;
    data->base    = buf;
    data->ptr     = buf + skip;
    data->ptr_end = buf + len;
}

/* allocate a comi Data and buffer (non growing) */
static Data* data_new(u_int8_t* buf, u_int32_t len, u_int32_t skip)
{
    Data* data = malloc(sizeof(Data)+len-1);
    if (data == NULL)
	return NULL;
    if (buf != NULL)
	memcpy(data->buf, buf, len);
    data->dynamic = 0;    /* otherwise reallocation wont work */
    data->base = data->buf;
    data->ptr = data->buf + skip;
    data->ptr_end = data->buf + len;
    return data;
}

static void data_final(Data* data)
{
    if (data->dynamic) {
	if (data->base) {
	    free(data->base);
	    data->base = NULL;
	}
    }
}

static void data_free(Data* data)
{
    data_final(data);
    free(data);
}
    

static void data_realloc(Data* data, u_int32_t need)
{
    u_int8_t* base   = data->base;
    int32_t offset;
    u_int32_t size = data->ptr_end - base;
    u_int32_t remain = data->ptr_end - data->ptr;
    u_int32_t new_size;

    if (remain >= need)
	return;
    if (need < 256)
	need += 256;
    new_size = size + need;
    if (data->dynamic)
	data->base = realloc(base, new_size);
    else {
	data->base = malloc(new_size);
	memcpy(data->base, base, size);
    }
    offset = data->base - base;
    data->ptr += offset;
    data->ptr_end += offset;
    data->ptr_end += need;
    data->dynamic = 1;
}

static inline u_int32_t data_avail(Data* data)
{
    return (data->ptr_end - data->ptr);
}

static inline u_int8_t* data_alloc(Data* data, u_int32_t len)
{
    u_int8_t* ptr;

    if (data_avail(data) < len)
	data_realloc(data, len);
    ptr = data->ptr;
    data->ptr += len;
    return ptr;
}

/* add "raw" data to Data buffer */
static inline void data_add(Data* data, u_int8_t* buf, u_int32_t len)
{
    u_int8_t* ptr = data_alloc(data, len);
    memcpy(ptr, buf, len);
}

/* skip "data" moving ptr forward */
static inline void data_forward(Data* data, u_int32_t len)
{
    /* u_int8_t* ptr = */ data_alloc(data, len);
    /* just use the side effect of data_alloc */
}

static inline void data_backward(Data* data, u_int32_t len)
{
    u_int8_t* ptr = data->ptr - len;
    if (ptr < data->base)
	data->ptr = data->base;
    else
	data->ptr = ptr;
}

static void data_send(Data* data, int fd)
{
    u_int32_t len = (data->ptr - data->base) - 4;
    PUT_UINT32(data->base, len);
    write(fd, data->base, len+4);
}

/*******************************************************************************
 *
 * PUT Untagged data
 *
 *******************************************************************************/

static inline void put_UINT8(Data* data, u_int8_t n)
{
    u_int8_t* ptr = data_alloc(data, 1);
    PUT_UINT8(ptr, n);
}

static inline void put_UINT16(Data* data, u_int16_t n)
{
    u_int8_t* ptr = data_alloc(data, 2);
    PUT_UINT16(ptr, n);
}

static inline void put_UINT32(Data* data, u_int32_t n)
{
    u_int8_t* ptr = data_alloc(data, 4);
    PUT_UINT32(ptr, n);
}

static inline void put_UINT64(Data* data, u_int64_t n)
{
    u_int8_t* ptr = data_alloc(data, 8);
    PUT_UINT64(ptr, n);
}

/*******************************************************************************
 *
 * PUT tagged data
 *
 *******************************************************************************/

static inline void put_boolean(Data* data, u_int8_t value)
{
    u_int8_t* ptr = data_alloc(data, 2);
    ptr[0] = BOOLEAN;
    ptr[1] = value;
}

static inline void put_int8(Data* data, int8_t n)
{
    u_int8_t* ptr = data_alloc(data, 2);
    *ptr++ = INT8;
    PUT_UINT8(ptr, n);
}

static inline void put_int16(Data* data, int16_t n)
{
    u_int8_t* ptr = data_alloc(data, 3);
    *ptr++ = INT16;
    PUT_UINT16(ptr, n);
}

static inline void put_int32(Data* data, int32_t n)
{
    u_int8_t* ptr = data_alloc(data, 5);
    *ptr++ = INT32;
    PUT_UINT32(ptr, n);
}

static inline void put_int64(Data* data, int64_t n)
{
    u_int8_t* ptr = data_alloc(data, 9);
    *ptr++ = INT64;
    PUT_UINT64(ptr, n);
}

static inline void put_float32(Data* data, float n)
{
    u_int8_t* ptr = data_alloc(data, 5);
    *ptr++ = FLOAT32;
    PUT_FLOAT32(ptr, n);
}

static inline void put_float64(Data* data, double n)
{
    u_int8_t* ptr = data_alloc(data, 9);
    *ptr++ = FLOAT64;
    PUT_FLOAT64(ptr, n);
}

static inline void put_uint8(Data* data, u_int8_t n)
{
    u_int8_t* ptr = data_alloc(data, 2);
    *ptr++ = UINT8;
    PUT_UINT8(ptr, n);
}

static inline void put_uint16(Data* data, u_int16_t n)
{
    u_int8_t* ptr = data_alloc(data, 3);
    *ptr++ = UINT16;
    PUT_UINT16(ptr, n);
}

static inline void put_uint32(Data* data, u_int32_t n)
{
    u_int8_t* ptr = data_alloc(data, 5);
    *ptr++ = UINT32;
    PUT_UINT32(ptr, n);
}

static inline void put_uint64(Data* data, u_int64_t n)
{
    u_int8_t* ptr = data_alloc(data, 9);
    *ptr++ = UINT64;
    PUT_UINT64(ptr, n);
}

/* put special tag like TUPLE/LIST/TUPLE_END/TUPLE_END */
static inline void put_tag(Data* data, u_int8_t tag)
{
    u_int8_t* ptr = data_alloc(data, 1);
    *ptr = tag;
}

static inline void put_atom(Data* data, const char* atom)
{
    u_int8_t* ptr;
    u_int32_t n = strlen(atom);

    if (n > 0xff)
	n = 0xff; /* truncate */
    ptr = data_alloc(data, n+2);
    *ptr++ = ATOM;
    *ptr++ = n;
    memcpy(ptr, atom, n);
}

static inline void put_string(Data* data, const char* string)
{
    if (string == NULL) {
	u_int8_t* ptr = data_alloc(data, 2);
	*ptr++ = STRING1;
	*ptr++ = 0;
    }
    else {
	u_int32_t n = strlen(string);
	if (n <= 0xff) {
	    u_int8_t* ptr = data_alloc(data, n+2);
	    *ptr++ = STRING1;
	    *ptr++ = n;
	    memcpy(ptr, string, n);
	}
	else {
	    u_int8_t* ptr = data_alloc(data, n+5);
	    *ptr++ = STRING4;
	    PUT_UINT32(ptr, n);
	    ptr += 4;
	    memcpy(ptr, string, n);
	}
    }
}

static inline void put_binary(Data* data, const u_int8_t* buf, u_int32_t len)
{
    u_int8_t* ptr = data_alloc(data, len+5);
    *ptr++ = BINARY;
    PUT_UINT32(ptr, len);
    ptr += 4;
    memcpy(ptr, buf, len);
}

/*******************************************************************************
 *
 * GET untagged data
 *
 *******************************************************************************/

static inline int get_boolean(Data* data, u_int8_t* val)
{
    if (data_avail(data) < sizeof(u_int8_t)) return 0;
    *val = (GET_UINT8(data->ptr) != 0);
    data->ptr += sizeof(u_int8_t);
    return 1;
}

static inline int get_uint8(Data* data, u_int8_t* val)
{
    if (data_avail(data) < sizeof(u_int8_t)) return 0;
    *val = GET_UINT8(data->ptr);
    data->ptr += sizeof(u_int8_t);
    return 1;
}

static inline int get_uint16(Data* data, u_int16_t* val)
{
    if (data_avail(data) < sizeof(u_int16_t)) return 0;
    *val = GET_UINT16(data->ptr);
    data->ptr += sizeof(u_int16_t);
    return 1;
}

static inline int get_uint32(Data* data, u_int32_t* val)
{
    if (data_avail(data) < sizeof(u_int32_t)) return 0;
    *val = GET_UINT32(data->ptr);
    data->ptr += sizeof(u_int32_t);
    return 1;
}

static inline int get_uint64(Data* data, u_int64_t* val)
{
    if (data_avail(data) < sizeof(u_int64_t)) return 0;
    *val = GET_UINT64(data->ptr);
    data->ptr += sizeof(u_int64_t);
    return 1;
}


