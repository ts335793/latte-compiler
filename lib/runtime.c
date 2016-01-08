#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void printInt(int i) {
    printf("%d\n", i);
}

void printString(char *s) {
    puts(s);
}
void error() {
    puts("runtime error");
    exit(-1);
}
void _no_return() {
    puts("no return statement");
    exit(-1);
}
char * readString() {
    char * line = NULL;
    size_t size;
    size_t last;
    getline(&line, &size, stdin);
    last = strlen(line) - 1;
    if (line[last] == '\n') line[last] = '\0';
    return line;
}
int readInt() {
    int i;
    scanf("%d", &i);
    getchar(); // read endline
    return i;
}

int _strlen(char * str) {
    return strlen(str);
}

char *_malloc(int s) {
    return malloc(s);
}

char *_strcat(char *a, char *b) {
    return strcat(a, b);
}

char *_strcpy(char *a, char *b) {
    return strcpy(a, b);
}

/*int main() {
    printInt(111);
    printString("ala ma kota");
    printInt(readInt());
    printString(readString());
    error();
    return 0;
}*/