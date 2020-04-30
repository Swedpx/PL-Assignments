#include "Transaction.h"

// default constructor
Transaction::Transaction() {

    _amount = _date = -1;
}

// constructor with parameters
Transaction::Transaction(double amount, time_t date){

    _amount = amount;
    _date = date;
}

// copy constructor
Transaction::Transaction(const Transaction& rhs){

    _date = rhs._date;
    _amount = rhs._amount;
}

// Compare two Transaction based on their date
bool Transaction::operator<(const Transaction& rhs) const {

    return difftime(_date, rhs._date) < 0;
}

// Compare two Transaction based on their date
bool Transaction::operator>(const Transaction& rhs) const {

    return difftime(_date, rhs._date) > 0;
}

// Compare a Transaction with a given date
bool Transaction::operator<(const time_t date) const {

    return difftime(_date, date) < 0;
}

// Compare a Transaction with a given date
bool Transaction::operator>(const time_t date) const {

    return difftime(_date, date) > 0;
}

// Sum the value of two Transaction amounts
double Transaction::operator+(const Transaction& rhs) {

    return _amount+rhs._amount;
}

// Sum the value of a Transaction with another double
double Transaction::operator+(const double add){

    return _amount+add;
}

// Assignment operator
Transaction& Transaction::operator=(const Transaction& rhs){

    _amount = rhs._amount;
    _date = rhs._date;

    return *this;
}

// Stream overload
std::ostream& operator<<(std::ostream& os, const Transaction& transaction){

    time_t localTime = transaction._date;
    struct tm* timeInfo = localtime (&localTime);

    os << transaction._amount << "\t-\t" << timeInfo->tm_hour << ":" << timeInfo->tm_min << ":" << timeInfo->tm_sec << "-" ;
    os << timeInfo -> tm_mday << "/" << timeInfo->tm_mon+1 << "/" << timeInfo->tm_year+1900 << "\n";

    return os;
}