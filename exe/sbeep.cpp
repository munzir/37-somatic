#include "somatic.h"
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char **argv) {
    (void) argc;
    int fd;
    if ((fd = open("/dev/tty1", O_WRONLY)) < 0) {
        fprintf(stderr, "Could not open /dev/console for writing.\n");
        printf("\a");
        perror("open");
        exit(1);
    }

    somatic_beep(fd, atof(argv[1]), 1);
    return 0;
}
