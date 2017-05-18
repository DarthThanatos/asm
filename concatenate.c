#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h> 
#include <stdbool.h>

#define BUFFER_LEN 100

#define CHECK(x) \
    do { \
        if (!(x)) { \
            fprintf(stderr,"%s %s:%d: ",__FILE__, __func__, __LINE__); \
            perror(#x); \
            exit(-1); \
        } \
    } while (0) \

typedef struct _node{
	char *name;
	char value[BUFFER_LEN];
	struct _node *next;
	int filelength;
	int index;
}node;

node *head, *tail, *p;

void init(int argc, char *argv[]){
	int fd;
	for (int i = 1; i < argc ; i++){
		p = malloc(sizeof (node));
		p->name = argv[i];
		CHECK( (fd = open(p->name, O_RDONLY)) != -1 );
		CHECK((p->filelength = read(fd, p->value, BUFFER_LEN)) != -1);
		p->value[p->filelength] = '\0';
		p->next = NULL;
		p->index = 0;
		if(head == NULL){
			head = p;
			tail = p;
		}
		else{
			tail-> next = p;
			tail = p;
		}
		CHECK(close(fd)!=-1);
	}
}

void run_concatenation(int argc){
	int files_served = 0;
	while(files_served != argc - 1){
		for (p = head; p!=NULL; p=p->next){
			while(p->index != p->filelength){
				if(p->index == p->filelength - 1) 
					files_served ++;
				if(p->value[p->index] == '\n') {
					p->index ++;
					break;
				}
				printf("%c", p->value[p->index++]);
			}
			if(p->next != NULL) printf(":");
			else printf("\n");
		}
	}
}

int main(int argc, char *argv[]){
	init(argc, argv);
	run_concatenation(argc);
}