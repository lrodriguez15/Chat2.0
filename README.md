# Chat2.0
Chat application using RTI DDS 

## Compile 

To compile it:
 
$ cd /home/$USER/ada/chat2.0
$ make -f make/Makefile.x64Linux2.6gcc4.4.5

For fully chat operation you need to open two terminals, one as a publisher and
one as a subscriber.

## Execute

To execute as a subscriber, this will allow you to read the messages of all the chat participants. 

$ cd /home/$USER/ada/chat2.0
$ ./bin/x64Linux2.6gcc4.4.5/chat-main sub 

To execute as a publisher, this will allow you to write messages to a chat room.

$ cd /home/$USER/ada/chat2.0
$ ./bin/x64Linux2.6gcc4.4.5/chat-main pub

In both cases, it will ask you to select one of the 3 chatrooms available.

You can change the domain ID as a publisher and subscriber to add multiple chatrooms of the same topic:

$ cd /home/$USER/ada/chat2.0
$ ./bin/x64Linux2.6gcc4.4.5/chat-main sub -d (number)

$ cd /home/$USER/ada/chat2.0
$ ./bin/x64Linux2.6gcc4.4.5/chat-main pub -d (number) 
