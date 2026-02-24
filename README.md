# SBW_Delphi

Many years ago I was involved with the development of the Systems Biology Workbench at Caltech. This was a system that allowed remote procedure calls into applications and represented one of the early adventures in top this area. The design was inspired by the music sharing application Napster. Our design was based on a broker model that ran in the background. An application could inspect services and methods exported by other application that was registered with the broker. If an aplication found a serivce it want to use it could make calls to the methods that were exposed. The messaging system was based on a efficient binary format that was transmitted via TCP/IP sockets. The broker was originally written in Java (2002), but Frank Bergmann rewrote the broker system in C++ in around 2004 which meant a user didn't have to have the Java runtime installed. A number of language binding libraries were written including C/C++, Java, Python, Delphi and Perl. This allowed applications written in different languages to communicate. 

I have been experimenting with the use of AI to code and wondered whether I could recreate the Broker, a Delphi binding library and a Python binding library. The python library is just one way in that it can also call out, nothing can call it. I also added some test modules to showing work.

It took me roughly 6 hours per day for 2 days to remake the software in Delphi, basically one of my weekends. I used Claude in the broswer, with a $20 per month subscription. I pointed Claude to the original source code as well as a couple of papers that described some of the internal systems especially the binary format. I have most of the system working. It probably took Frank 3 months to create the C++ version but ony 2 days to create the Delphi version using AI.

It's probably of not much interest to others but if anyone is, the project can be loaded from the group 

ProjectGroupSBW.groupproj

This should work with the community edition of Delphi which is free to hobbyists and students.

I have included a math client and server porject in the group project as well as a simple command line browser  tool, also written by the AI.

The other modules have their own separate projects. 
