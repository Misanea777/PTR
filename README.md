<<<<<<< HEAD
 PTR# REAL TIME PROGRAMMING
=======
# PTR# REAL TIME PROGRAMMING
>>>>>>> 1d0935ca3cf0a6c9399c5a470cf3b23df70517b0
### Laboratory work 1
### Student Filipescu Mihail
#### Group: FAF-181

<br>
<hr>

## Tasks:
- You will have to read 2 SSE streams of actual Twitter API tweets in JSON format.
- The streams are available from a Docker container, alexburlacu/rtp-server:faf18x, just like Lab 1 PR, only now it's on port 4000.
- To make things interesting, the rate of messages varies by up to an order of magnitude, from 100s to 1000s.
- Then, you route the messages to a group of workers that need to be autoscaled, you will need to scale up the workers (have more) when the rate is high, and less actors when the rate is low.
- Route/load balance messages among worker actors in a round robin fashion.
- Occasionally you will receive "kill messages", on which you have to crash the workers.
- To continue running the system you will have to have a supervisor/restart policy for the workers.
- The worker actors also must have a random sleep, in the range of 50ms to 500ms, normally distributed. This is necessary to make the system behave more like a real one + give the router/load balancer a bit of a hard time + for the optional speculative execution. The output will be shown as log messages.
### Optional tasks:
- Speculative execution of slow tasks, for some details check the recording of the first lesson.
- "Least connected" load balancing, or even more advanced, check for some examples (not directly related):[here](https://blog.envoyproxy.io/examining-load-balancing-algorithms-with-envoy-1be643ea121c).
- Have a metrics endpoint to monitor the stats on ingested messages, average execution time, 75th, 90th, 95th percentile execution time, number of crashes per given time window, etc.
- Anything else, like the most popular hashtag up until now, or maybe other analytics.

## Run

```
erl -make
erl -pa ebin
main_sup:start_link().
```

## Demonstration:
<<<<<<< HEAD
https://www.youtube.com/watch?v=hAs9p6QPZ6I
=======
https://www.youtube.com/watch?v=hAs9p6QPZ6I
>>>>>>> 1d0935ca3cf0a6c9399c5a470cf3b23df70517b0
