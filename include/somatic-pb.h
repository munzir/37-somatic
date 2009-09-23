
#define PROTOBUF_FUNCTION_PREFIX static
#define PROTOBUF_DESCRIPTOR_PREFIX static

#define SOMATIC_STATE_ARRAY(msg) { msg.x, msg.y, msg.z, msg.qx, msg.qy, msg.qz, msg.qw }
#define SOMATIC_STATE_POS(msg) { msg.x, msg.y, msg.z }
#define SOMATIC_STATE_ROT(msg) { msg.qx, msg.qy, msg.qz, msg.qw }

// So, realistically these should maybe be functions, not macros... Need to see more of their actual use cases
#define SOMATIC_SET_STATE_ARRAY(msg, arr) { \
    msg.x = arr[0]; \
    msg.y = arr[1]; \
    msg.z = arr[2]; \
    msg.qx = arr[3]; \
    msg.qy = arr[4]; \
    msg.qz = arr[5]; \
    msg.qw = arr[6]; \
}

#define SOMATIC_SET_STATE_POS(msg, arr) { \
    msg.x = arr[0]; \
    msg.y = arr[1]; \
    msg.z = arr[2]; \
}

#define SOMATIC_SET_STATE_ROT(msg, arr) { \
    msg.qx = arr[0]; \
    msg.qy = arr[1]; \
    msg.qz = arr[2]; \
    msg.qw = arr[3]; \
}

#include <somatic/somatic.pb-c.h>

