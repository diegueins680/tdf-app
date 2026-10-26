/* Test-only Darwin adapter. Production executable and source stay unchanged.
 * The selected API port must bind loopback; the launcher independently checks
 * the actual listener and rejects LAN access before opening a browser. */
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <string.h>
static int pilot_bind(int fd, const struct sockaddr *address, socklen_t size) {
  const char *raw = getenv("TDF_PILOT_API_PORT");
  unsigned long port = raw ? strtoul(raw, 0, 10) : 0;
  if (address && address->sa_family == AF_INET && size >= sizeof(struct sockaddr_in)) {
    struct sockaddr_in local; memcpy(&local, address, sizeof local);
    if (port && ntohs(local.sin_port) == port) {
      local.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
      return bind(fd, (struct sockaddr *)&local, sizeof local);
    }
  }
  return bind(fd, address, size);
}
__attribute__((used)) static struct { const void *replacement; const void *original; }
  interpose_bind __attribute__((section("__DATA,__interpose"))) =
  { (const void *)pilot_bind, (const void *)bind };
