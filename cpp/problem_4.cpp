/**
 * Largest palindrome product
 * Problem 4
 *
 * A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
 *
 * Find the largest palindrome made from the product of two 3-digit numbers.
 *
 */

#include <iostream>
#include <vector>
#include <string>
#include <cmath>

namespace
{

/**
 *
 */
bool isPalindrome(int x)
{
    std::string str = std::to_string(x);
    for (size_t i = 0; i < str.size() / 2; ++i)
    {
        size_t headIndex = i;
        size_t tailIndex = str.size() - 1 - i;
//        std::cout << "head: " << str[headIndex] << ", tail: " << str[tailIndex] << std::endl;
        if (str[headIndex] != str[tailIndex])
            return false;
    }

    return true;
}

///**
// *
// * @param integer number
// * @return two prime numbers of consisting the parameter.
// *         two prime numbers range is [100 ~ 999].
// *         if the parameter does not compose of two prime numbers,
// *         or if prime numbers range is out of [100 ~ 999],
// *         then return no values.
// */
//std::vector<int> decomposeTwoPrimes(int x)
//{
//    const int min = 100;
//    const int max = 999;
//
//    std::vector<int> primeFactors;
//    for(int i= min; i< max; ++i)
//    {
//        if(x % i != 0)
//            continue;
//
//        int quotient = x / i;
//        if(quotient >= 100 && quotient <= 999)
//            return std::vector<int>{i, quotient};
//    }
//
//    return std::vector<int>{};
//}

struct TwoPrimes{
    int first;
    int second;
};
/**
 *
 * @param integer number
 * @return two prime numbers of consisting the parameter.
 *         two prime numbers range is [100 ~ 999].
 *         if the parameter does not compose of two prime numbers,
 *         or if prime numbers range is out of [100 ~ 999],
 *         then return no values.
 */
std::vector<TwoPrimes> decomposeTwoPrimes(int x)
{
    const int min = 100;
    const int max = 999;

    std::vector<TwoPrimes> primeFactors;
    for(int i= min; i< max && i < std::sqrt(x); ++i)
    {
        if(x % i != 0)
            continue;

        int quotient = x / i;
        if(quotient >= 100 && quotient <= 999)
            primeFactors.push_back(TwoPrimes{i, quotient});
    }

    return primeFactors;
}

} // namespace

namespace problem_4
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
    int maxNum = 999 * 999;
    std::cout << "max number: " << maxNum << std::endl;
    for (int i = maxNum; i >= 0; --i)
    {
        //----------------------------------
        // create a palindromic number
        //----------------------------------
        if (!isPalindrome(i))
            continue;

        std::cout << "palindrome: " << i << std::endl;

        //----------------------------------
        //
        //----------------------------------
        std::vector<TwoPrimes> primeFactors = decomposeTwoPrimes(i);
        if(primeFactors.size() > 0)
        {
            std::cout << "result: " << i << std::endl;
            std::cout << "prime factors: "<< primeFactors[0].first << " * " << primeFactors[0].second << " = " << primeFactors[0].first * primeFactors[0].second << std::endl;
            break;
        }

    }

    //
    std::cout << "finished " << __FUNCTION__ << std::endl;

    return 0;
}

} // namespace problem_4
