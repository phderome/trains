To build:

Use sbt 1.2.6

$ sbt run

$ sbt test
 
To have a more pleasant test experience, build from Intellij IDEA and run tests.
 
Sample run:

MacBook-Pro-3:trains philippederome$ ls

README.md	Trains.ipr	Trains.iws	build.sbt	project		src		target

MacBook-Pro-3:trains philippederome$ sbt run

[info] Loading settings for project global-plugins from idea.sbt ...

... skipped

[info] Running com.deromefintech.trains.TrainApp 

enter graph for example Graph: AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7

Graph: AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7

Output #1: 9

Output #2: 5

Output #3: 13

Output #4: 22

Output #5: NO SUCH ROUTE

Output #6: 2

Output #7: 3

Output #8: 9

Output #9: 9

Output #10: 7

[success] Total time: 16 s, completed 23-Oct-2018 7:39:50 AM

MacBook-Pro-3:trains philippederome$ sbt test

[info] Loading settings for project global-plugins from idea.sbt ...

... skipped

[info] Done compiling.

[info] routeservicespec

[info] routeservice must

[info]   + Number of findWalksMaxHops is equal to the sum of findWalksExact for all values at or below its limit  + findWalksMaxHops do not stop more than promised  + findWalksExact do not stop more or less than promised  + findWalksExact computes a walk in the graph that library recognizes as such  + exploreWalksWithinDistance does not retain paths above the allowed limit  + exploreWalksWithinDistance allows a single weight to be within its limit  + getDistance when walk is cyclic, all nodes on cycle have same or less distance to themselves  + getDistance evaluates the same using brute force  + getDistance evaluates to number of edges in a graph of weight 1  + shortestSame is defined iff start node is in a cycle that includes itself outgoing and incoming (as opposed to on a path entering a cycle)

[info] Total for specification routeservicespec

[info] 10 examples, 1000 expectations, 0 failure, 0 error

[info] Passed: Total 10, Failed 0, Errors 0, Passed 10

[success] Total time: 10 s, completed 23-Oct-2018 7:40:14 AM
