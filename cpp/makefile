
#--------------------------------------
TARGET = project_euler

HEADERS = solvers.h

OBJS = main.o \
       problem_1.o \
       problem_2.o \
       problem_4.o \
       problem_5.o \

CPPFLAGS = -Wall -O0 -g --std=c++11
#CPPFLAGS = -Wall -O3 --std=c++11


#--------------------------------------
.PHONY: all clean

#--------------------------------------
all:
	make $(TARGET) && make run

#--------------------------------------
$(TARGET): $(OBJS)
	$(CXX) $(CPPFLAGS) -o $@ $^

#--------------------------------------
SUFFIXES: .o .cpp
.cpp.o: $(HEADERS)
	$(CXX) $(CPPFLAGS) -c $<

#--------------------------------------
clean:
	rm $(TARGET)
	rm *.o

run:
	./$(TARGET)


