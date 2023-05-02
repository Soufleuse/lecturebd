SRC = lecturebd
COPYBOOKS = /home/lemste/src/dbpre_0.4/copybooks
INTERMEDIATE = lecturebd.cob
OBJ = $(SRC:.cc=.o)
EXEC = lecturebd

all:
	dbpre -I$(COPYBOOKS) $(SRC)
	cobc -x $(INTERMEDIATE) cobmysqlapi.o -L/usr/include/mysql -lmysqlclient -I/home/lemste/src/dbpre_0.4/copybooks

clean:
	rm -rf $(OBJ) $(EXEC)
