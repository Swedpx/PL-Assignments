#include "Account.h"

void swap(Transaction* lhs, Transaction* rhs)  
{  
    Transaction temp = *lhs;

    *lhs = *rhs;
    *rhs = temp;
}  

void sort(Transaction* ptr, int size){

    for (int i = 0; i < size -1; i++)

        for (int j=0; j < size-i-1; j++)

            if(ptr[j] > ptr[j+1])

                swap(ptr+j, ptr+j+1);

    
}

// default constructor
Account::Account(){
    
    _id = -1;
    _activity = nullptr;
    _monthly_activity_frequency = nullptr;
}

// constructor with parameters
Account::Account(int id, Transaction** const activity, int* monthly_activity_frequency) {

    if (activity == nullptr || monthly_activity_frequency == nullptr) {
        
        _activity = nullptr;
        _monthly_activity_frequency = nullptr;
        _id = id;
        return;
    }

    _id = id;
    _activity = new Transaction*[12];
    _monthly_activity_frequency = new int[12];

    for (int i=0; i < 12; i++){

        int transNumber = monthly_activity_frequency[i];

        if (transNumber == 0) {_activity[i] = nullptr; _monthly_activity_frequency[i] = 0; continue;}

        _activity[i] = new Transaction[transNumber];

        for (int j=0; j < transNumber; j++)
            _activity[i][j] = activity[i][j];

        sort(_activity[i], transNumber);

        _monthly_activity_frequency[i] = monthly_activity_frequency[i];
    }
}

// destructor
Account::~Account(){

    if (_activity != nullptr){
        for (int i=0; i < 12; i++)
            if (_activity[i] != nullptr)
                delete [] _activity[i];

        delete [] _activity;
    }
    if (_monthly_activity_frequency != nullptr) delete [] _monthly_activity_frequency;
}

// copy constructor #1
Account::Account(const Account& rhs){

    if (this == &rhs || rhs._monthly_activity_frequency == nullptr || rhs._activity == nullptr) return;

    _activity = new Transaction*[12];
    _monthly_activity_frequency = new int[12];
    _id = rhs._id;

    for (int i=0; i < 12; i++){

        int transactionNumber = rhs._monthly_activity_frequency[i];

        if (transactionNumber == 0) {_activity[i] = nullptr; _monthly_activity_frequency[i] = 0; continue;}
        
        _activity[i] = new Transaction[transactionNumber];

        for (int j=0; j < transactionNumber; j++)
            _activity[i][j] = rhs._activity[i][j];

        _monthly_activity_frequency[i] = rhs._monthly_activity_frequency[i];
    }
}

// copy constructor #2
Account::Account(const Account& rhs, time_t start_date, time_t end_date) {

    if (this == &rhs || rhs._monthly_activity_frequency == nullptr || rhs._activity == nullptr) return;

    _activity = new Transaction*[12];
    _monthly_activity_frequency = new int[12];
    _id = rhs._id;

    for (int i=0; i < 12; i++){

        int transactionNumber = 0;

        for (int j=0; j < rhs._monthly_activity_frequency[i]; j++) // if 0 transactions are done then loop won't be executed so no problem with nullptr.
            if (rhs._activity[i][j] < end_date && rhs._activity[i][j] > start_date) transactionNumber++;
            
        if (transactionNumber == 0) {_activity[i] = nullptr; _monthly_activity_frequency[i] = 0; continue;}
        
        _activity[i] = new Transaction[transactionNumber];

        int j=0;

        for (int k=0; k < rhs._monthly_activity_frequency[i]; k++)

            if (rhs._activity[i][k] > start_date && rhs._activity[i][k] < end_date){
                _activity[i][j] = rhs._activity[i][k];
                j++;
            }

        _monthly_activity_frequency[i] = transactionNumber;
    }
}

// move constuctor
Account::Account(Account&& rhs){

    if (this == &rhs) return;

    _id = rhs._id;
    _activity = rhs._activity;
    _monthly_activity_frequency = rhs._monthly_activity_frequency;

    rhs._id = -1;
    rhs._activity = nullptr;
    rhs._monthly_activity_frequency = nullptr;
}

// move assignment operator
Account& Account::operator=(Account&& rhs){

    if (this != &rhs){

        _id = rhs._id;
        _activity = rhs._activity;
        _monthly_activity_frequency = rhs._monthly_activity_frequency;

        rhs._id = -1;
        rhs._activity = nullptr;
        rhs._monthly_activity_frequency = nullptr;

    }
    return *this;
}

// assignment operator
Account& Account::operator=(const Account& rhs){

    if (this != &rhs && rhs._activity != nullptr && rhs._monthly_activity_frequency != nullptr){

        _id = rhs._id;

        if (_activity != nullptr) delete [] _activity;
        if (_monthly_activity_frequency != nullptr) delete [] _monthly_activity_frequency;
        
        _activity = new Transaction*[12];
        _monthly_activity_frequency = new int[12];

        for (int i=0; i < 12; i++){

            int transNumber = rhs._monthly_activity_frequency[i];

            if (transNumber == 0) { _activity[i] = nullptr; _monthly_activity_frequency[i] = 0; continue; }

            _activity[i] = new Transaction[transNumber];

            for (int j=0; j < transNumber; j++)
                _activity[i][j] = rhs._activity[i][j];

            _monthly_activity_frequency[i] = rhs._monthly_activity_frequency[i];
        }
    }
    return *this;
}

// comparison operator overloads
bool Account::operator==(const Account& rhs) const { return _id == rhs._id; }
bool Account::operator==(int id) const { return _id == id; }

// sum and equal operator
Account& Account::operator+=(const Account& rhs){

    if (rhs._activity != nullptr && rhs._monthly_activity_frequency != nullptr){

        for (int i=0; i < 12; i++){

            int newTransNumber = _monthly_activity_frequency[i] + rhs._monthly_activity_frequency[i];

            if (newTransNumber == 0) { _activity[i] = nullptr; _monthly_activity_frequency[i] = 0; continue;}

            Transaction* tmp = new Transaction[newTransNumber];

            for (int j=0; j < newTransNumber; j++){
                
                if (j < _monthly_activity_frequency[i]) tmp[j] = _activity[i][j];
                else{
                    int index = j - _monthly_activity_frequency[i];
                    tmp[j] = rhs._activity[i][index];
                }
            }

            delete [] _activity[i];
            sort(tmp, newTransNumber);
            _activity[i] = tmp;
            _monthly_activity_frequency[i] = newTransNumber;
        }
    }
    return *this;
}

double Account::balance(){

    double sum=0;

    for (int i=0; i < 12; i++){

        int transNumber = _monthly_activity_frequency[i];

        for (int j=0; j < transNumber; j++) sum = _activity[i][j] + sum;
    }
    return sum;
}

double Account::balance(time_t end_date){

    double sum=0;
    bool done=false;

    for (int i=0; i < 12; i++){

        for (int j=0; j < _monthly_activity_frequency[i]; j++){

            if (_activity[i][j] < end_date) sum = _activity[i][j] + sum;
            else {done = true; break;}

        }
        if (done) break;
    }
    return sum;
}

double Account::balance(time_t start_date, time_t end_date){

    double sum=0;
    bool done=false;

    for (int i=0; i < 12; i++){

        for (int j=0; j < _monthly_activity_frequency[i]; j++){

            if (_activity[i][j] > start_date){
                if (_activity[i][j] < end_date) sum = _activity[i][j] + sum;
                else {done = true; break;}
            }
        }
        if (done) break;
    }
    return sum;
}

// Stream overload
std::ostream& operator<<(std::ostream& os, const Account& account){

    if (account._activity == nullptr || account._monthly_activity_frequency == nullptr) {os << "-1\n"; return os;}

    os << account._id << "\n";
    
    for (int i=0; i < 12; i++){

        int transNumber = account._monthly_activity_frequency[i];

        for (int j=0; j < transNumber; j++)
            os << account._activity[i][j];
    }
    return os;
}
