/**
 * Smallest multiple
 * Problem 5
 *
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 *
 * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 *
 */

#include <iostream>
#include <vector>
#include <map>
#include <cmath>

namespace
{

/**
 *
 */
int getMinimumPrime(int n)
{
    // TODO: error handling

    //
    for (int i = 2; i < n; ++i)
        if (n % i == 0)
            return i;
    return n;
}

/**
 *
 */
std::vector<int> decomposePrimeFactors(int n)
{
    // TODO: error handling

    //
    std::vector<int> result;

    for (int current = n; current != 1;)
    {
        int prime = getMinimumPrime(current);
        result.push_back(prime);
        current /= prime;
    }
    return result;
}

std::map<int, int> countPrimeFactors(const std::vector<int> primes)
{
    std::map<int, int> result;
    for (int e : primes)
        ++result[e];
    return result;
}

} // namespace

namespace problem_5
{

/**
 *
 */
int solve()
{
    //
    std::cout << "started " << __FUNCTION__ << std::endl;

    //----------------------------------
    //
    //----------------------------------
    std::cout << "prime factors: " << std::endl;
    const int maxNumber = 20;
    std::map<int, int> maxPrimeCounts;
    for (int i = 2; i <= maxNumber; ++i)
    {
        //
        std::vector<int> primes = decomposePrimeFactors(i);
//        std::cout << i << ": ";
//        for (int e : primes)
//            std::cout << e << ", ";
//        std::cout << std::endl;

        //
        std::map<int, int> primeCount = countPrimeFactors(primes);
//        std::cout << i << ": ";
//        for (std::pair<int, int> e : primeCount)
//            std::cout << e.first << " -> " << e.second << ", ";
//        std::cout << std::endl;

        //
        for (std::pair<int, int> prime2count : primeCount)
        {
            int key = prime2count.first;
            int newval = prime2count.second;
            if (maxPrimeCounts.count(key))
            {
                int curval = maxPrimeCounts[key];
                maxPrimeCounts[key] = std::max(curval, newval);
            }
            else
            {
                maxPrimeCounts.insert(std::make_pair(key, newval));
            }
        }

    }
    //
    for (std::pair<int, int> e : maxPrimeCounts)
        std::cout << e.first << " -> " << e.second << std::endl;
    std::cout << std::endl;

    //----------------------------------
    //
    //----------------------------------
    int product = 1;
    for (std::pair<int, int> e : maxPrimeCounts)
        product *= std::pow(e.first, e.second);
    std::cout << "result: " << product << std::endl;

    //
    std::cout << "finished " << __FUNCTION__ << std::endl;

    return 0;
}

} // namespace problem_5
