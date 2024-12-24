/* cc -g matrixcalculus.c -o matrixcalculus
 */

// TODO: Win32 port.
// https://learn.microsoft.com/it-it/windows/win32/api/winsock2/nf-winsock2-send
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/socket.h>
#include <unistd.h>

int main() {
    int sockfd = -1;
    {
        // https://w3.cs.jmu.edu/kirkpams/OpenCSF/Books/csf/html/UDPSockets.html
        int err = 0;
        struct addrinfo
            hint = {
                .ai_flags = 0,
                .ai_family = AF_INET,
                .ai_socktype = SOCK_STREAM,
                .ai_protocol = IPPROTO_TCP,
            },
            *sentinel = &(struct addrinfo){0}, // socket(0, 0, 0) == -1
            *addrinfo_head = sentinel;

        err = getaddrinfo("www.matrixcalculus.org", "80", &hint, &addrinfo_head);
        if (err) fprintf(stderr, "%s\n", gai_strerror(err));
        for (struct addrinfo *addrinfo = addrinfo_head;
            addrinfo;
            addrinfo = addrinfo->ai_next) {

            sockfd = socket(addrinfo->ai_family, addrinfo->ai_socktype, addrinfo->ai_protocol);
            if (sockfd == -1) fprintf(stderr, "%s\n", strerror(errno));
            err = connect(sockfd, addrinfo->ai_addr, addrinfo->ai_addrlen);
            if (err == -1) {
                fprintf(stderr, "%s\n", strerror(errno));
                close(sockfd);
                sockfd = -1;
                continue;
            }
            break;
        }
        if (addrinfo_head != sentinel) freeaddrinfo(addrinfo_head);

        struct timeval timeout = { .tv_usec = 500000 }; // Half a second
        err = setsockopt(sockfd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof timeout);
        if (err == -1) fprintf(stderr, "%s\n", strerror(errno));
        err = setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof timeout);
        if (err == -1) fprintf(stderr, "%s\n", strerror(errno));
        // TODO: return invalid sockfd if we can't set timeouts.
    }


    static char request[1024];

    const char *content =
        "{\n"
        "    \"expression\": \"x'*A*x + c*sin(y)'*x\",\n"
        "    \"wrt\": {\"name\": \"x\", \"type\": \"vector\"},\n"
        "    \"varList\": [\n"
        "        {\"name\": \"A\", \"type\": \"symmetric matrix\"},\n"
        "        {\"name\": \"c\", \"type\": \"scalar\"},\n"
        "        {\"name\": \"x\", \"type\": \"vector\"},\n"
        "        {\"name\": \"y\", \"type\": \"vector\"}\n"
        "    ],\n"
        "    \"n\": 4\n"
        "}\n"
    ;
    snprintf(
        request,
        sizeof request,
        "POST /_show HTTP/1.1\r\n"
        "Host: www.matrixcalculus.org\r\n"
        "User-Agent: someone\r\n"
        "Accept: */*\r\n"
        "Content-Type: application/json\r\n"
        "Connection: close\r\n" // https://stackoverflow.com/a/17438094
        // TODO: set timeout if not supported.
        "Content-Length: %zu\r\n"
        "\r\n"
        "%s",
        strlen(content),
        content
    );
    ssize_t transmitted_bytes = 0;
    transmitted_bytes = send(sockfd, request, strlen(request), 0);
    if (transmitted_bytes == -1) fprintf(stderr, "%s\n", strerror(errno));
    static char buffer[2048];
    while ((transmitted_bytes = recv(sockfd, buffer, sizeof buffer - 1, 0)) > 0) {
        buffer[transmitted_bytes] = '\0';
        printf("%s", buffer);
        // https://www.json.org/json-en.html
    }
    if (transmitted_bytes == -1) fprintf(stderr, "%s\n", strerror(errno));

    // https://blog.netherlabs.nl/articles/2009/01/18/the-ultimate-so_linger-page-or-why-is-my-tcp-not-reliable

    // close(sockfd);
    return 0;
}