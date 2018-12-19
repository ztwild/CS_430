package histogram

import (
	"math/rand"
	"time"
	"fmt"
)

type Hist struct {
	Bins       []int
	NumSamples int
}

func makeRand() *rand.Rand {
	return rand.New(rand.NewSource(time.Now().UTC().UnixNano()))
}

func New(upperBound int) Hist {
	return Hist{
		Bins:       make([]int, upperBound),
		NumSamples: 0,
	}
}

func (h *Hist) merge(other Hist) (err bool) {
	for i, v := range other.Bins {
		(*h).Bins[i] += v
	}
	return true
}

func Make(numSamples, upperBound int) Hist {
	rand := *makeRand()
	h := New(upperBound)
	h.NumSamples = numSamples
	for i := 0; i < numSamples; i++ {
		rand_index := rand.Intn(upperBound)
		h.Bins[rand_index] += 1
	}
	return h
}

func histogram_channel(numSamples, upperBound int, out chan Hist ){
	h := Make(numSamples, upperBound)
	out <- h
}

// Makes a histogram using the indicated number of workers running in parallel.
func MakeInParallel(numSamples, upperBound, numWorkers int) Hist {
	master_histogram := New(upperBound)
	c := make(chan Hist)
	portionSamples := numSamples / numWorkers
	
	for i := 0; i < numWorkers; i++ {
		go histogram_channel(portionSamples, upperBound, c)
	}

	count := 0
	for{
		select{
		case other := <- c:
			master_histogram.merge(other)
			
			count++
			if(count == numWorkers){
				fmt.Printf("Returning Master Histogram\n")
				close(c)
				return master_histogram
			}
		}
	}
}




