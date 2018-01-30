To compile the demos you need synapse, synwrap1, strwrap1. 
These should be included in the zip in the demos/requires/ folder
Add search paths to your fpc.cfg file or your ide -Fu settings.

Compiled with FPC 2.1.4 but should work with 2.0.4 and others

One of the demos shows how to recursively extract urls from web pages 
(traversing through until no more links found)

The other demos are simpler such as extracting urls from a single page
and some tests to verify the htmlutil unit works properly.
