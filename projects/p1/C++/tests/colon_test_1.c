#include <stdio.h>

int colon_test_1(int x, int y, int z);

int colon_test_1_tester(int x, int y, int z) {
    return (z<<1) | (y<<4) | (x<<9);
}


int main() {
    for (int x = 0; x < 256; x++) {
        for (int y = 0; y < 256; y++) {
            for (int z = 0; z < 256; z++) {
                if (colon_test_1(x, y, z) != colon_test_1_tester(x, y, z)) {
                    fprintf(
                        stderr,
                        "colon_test_1(%0d, %0d, %0d) returned %0d, but %0d was expected\n",
                        x, y, z,
                        colon_test_1(x, y, z),
                        colon_test_1_tester(x, y, z)
                    );
                    return 1;
                }
            }
        }
    }

    return 0;
}
