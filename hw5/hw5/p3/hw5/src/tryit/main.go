package main

import (
	"fmt"
	"histogram"
	"time"
)

func main() {
	start := time.Now()
	h := histogram.Make(
	  DefaultNumSamples,
		DefaultUpperBound,
	)
	elapsed := time.Since(start)
	fmt.Printf("Elapsed: %v\n", elapsed)
	total_samples := 0
	for samples := range h.Bins {
		//fmt.Printf("%v : %v\n", bin, samples)
		total_samples += h.Bins[samples]
	}
	fmt.Printf("Total number of samples:  %v\n", total_samples)

	start = time.Now()
	h = histogram.MakeInParallel(
		DefaultNumSamples,
		DefaultUpperBound,
    DefaultNumWorkers,
	)
	elapsed = time.Since(start)
	fmt.Printf("Elapsed: %v\n", elapsed)

	total_samples = 0
	for samples := range h.Bins {
		//fmt.Printf("%v : %v\n", bin, samples)
		total_samples += h.Bins[samples]
	}
	fmt.Printf("Total number of samples:  %v\n", total_samples)
}
