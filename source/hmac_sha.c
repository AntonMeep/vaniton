#include <openssl/hmac.h>
#include <openssl/sha.h>
#include <stdbool.h>
#include <stdlib.h>

bool hmac_sha512(const uint8_t* key, size_t key_len, const uint8_t* message,
                 size_t message_length, uint8_t* out, size_t nout) {
  unsigned int len = 0;
  if (HMAC(EVP_sha512(), key, key_len, message, message_length, out, &len) ==
      NULL) {
    return false;
  }

  if (len != nout) {
    return false;
  }

  return true;
}
