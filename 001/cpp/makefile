
TARGET = project_euler

OBJS = main.o

CPPFLAGS = -Wall -O0 -g --std=c++11

.PHONY: all clean

all:
	make cpp

cpp:
	make $(TARGET) \
	&& make run \
	&& make clean

$(TARGET): $(OBJS)
	$(CXX) $(CPPFLAGS) -o $@ $^

SUFFIXES: .o .cpp
.cpp.o:
	$(CXX) $(CPPFLAGS) -c $<

clean:
	rm $(TARGET)
	rm *.o

run:
	./$(TARGET)


