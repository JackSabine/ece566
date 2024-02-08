#include <stdio.h>

int colon_test_2(int x);

int colon_test_2_tester(int x) {
    return x << 3;
}


int main() {
    for (int x = 0; x < (1 << 16); x++) {
        if (colon_test_2(x) != colon_test_2_tester(x)) {
            fprintf(
                stderr,
                "colon_test_2(%0d) returned %0d, but %0d was expected\n",
                x,
                colon_test_2(x),
                colon_test_2_tester(x)
            );
            return 1;
        }
    }

    return 0;
}
