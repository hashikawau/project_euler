/**
 * Multiples of 3 and 5
 * Problem 1
 *
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *
 */

#include <iostream>
#include <vector>

namespace
{

/**
 *
 */
//bool isMultipleOfPrimes(int x)
//{
//    return (x % 3 == 0) || (x % 5 == 0);
//}
/**
 *
 */
bool isMultipleOfPrimes(int x, const std::vector<int>& primeNumbers =
{ 3, 5 })
{
    for (int primeNumber : primeNumbers)
        if (x % primeNumber == 0)
            return true;

    return false;
}

/**
 *
 */
int addMultiplesOfPrimes(int maxNumber)
{
    int sum = 0;
    for (int i = 0; i < maxNumber; ++i)
        if (isMultipleOfPrimes(i))
            sum += i;

    return sum;
}

} // namespace

namespace problem_1
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
    const int maxNumber = 1000;
    const int sum = addMultiplesOfPrimes(maxNumber);
    std::cout << "result: " << sum << std::endl;

    //
    std::cout << "finished " << __FUNCTION__ << std::endl;

    return 0;
}

} // namespace problem_1
