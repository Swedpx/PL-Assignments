#include "Bank.h"

Bank::Bank(){ _bank_name = "not_defined"; _user_count = 0; _users = nullptr; }

Bank::Bank(std::string bank_name, Account* const users, int user_count) {

    _bank_name = bank_name;
    _user_count = user_count;
    _users = new Account[user_count];

    for (int i=0; i < user_count; i++) _users[i] = users[i];
}

Bank::~Bank(){ if (_users != nullptr) delete [] _users; }

Bank::Bank(const Bank& rhs){

    _bank_name = rhs._bank_name;
    _user_count = rhs._user_count;
    _users = new Account[_user_count];

    for (int i=0; i < _user_count; i++) _users[i] = rhs._users[i];
}

template <class T>
Account* find (Account* ptr, int size, const T& acc){

    for (int i=0; i < size; i++)
        if (ptr != nullptr && ptr[i] == acc) return ptr+i;

    return nullptr;
}

Bank& Bank::operator+=(const Bank& rhs){

    if (rhs._user_count != 0){ // what if _users == nullptr?

        int newCount = 0;

        for (int i=0; i < rhs._user_count; i++)
            if (find(_users, _user_count, rhs._users[i]) == nullptr) newCount++;

        Account* tmp = new Account[_user_count + newCount];
        
        for (int i=0; i < _user_count+newCount; i++) tmp[i] = Account();

        for (int i=0; i < _user_count; i++) tmp[i] = _users[i];

        int i = _user_count;

        for (int j=0; j < rhs._user_count; j++){

            if (find(tmp, _user_count+newCount, rhs._users[j]) != nullptr){
                
                Account& ref = *(find(tmp, _user_count+newCount, rhs._users[j]));
                ref += rhs._users[j];
            }
            else tmp[i++] = rhs._users[j];
        }
        
        if (_users != nullptr)
            delete [] _users;

        _users = tmp;
        _user_count += newCount;
    }
    return *this;
}

Bank& Bank::operator+=(const Account& new_acc){

    if (find(_users, _user_count, new_acc) == nullptr){

        Account* tmp = new Account[_user_count+1];

        for (int i=0; i < _user_count; i++) tmp[i] = _users[i];

        tmp[_user_count++] = new_acc;
        
        delete [] _users;
        _users=tmp;
        
    } else{
        Account& ref = *(find(_users, _user_count, new_acc));
        ref += new_acc;
    }
    return *this;
}

Account& Bank::operator[](int account_id){

    if (_users != nullptr)
        return find(_users, _user_count, account_id) == nullptr ? _users[0]: *(find(_users, _user_count, account_id));
}

std::ostream& operator<<(std::ostream& os, const Bank& bank){

    int total_balance = 0;
    int account_count = 0;

    for (int i=0; i < bank._user_count; i++){

        total_balance += bank._users[i].balance();
        
        bool not_eligible = false;
        int consec_minus = 0;

        struct tm start = {0};
        struct tm end = {0};

        start.tm_year = 119; start.tm_mday = 1; start.tm_mon = 0;
        end.tm_year = 119; end.tm_mday = 31; end.tm_hour = 23; end.tm_min = 59; end.tm_sec = 59; end.tm_mon = 0;

        for (int j=0; j <12; j++){
            
            if (j == 0 || j == 2 || j==4 || j==6 || j==7 || j==9 || j==11) end.tm_mday = 31;
            else if (j == 3 || j==5 || j==8 || j==10 || j==12) end.tm_mday = 30;
            else end.tm_mday = 28;

            double balance = bank._users[i].balance(mktime(&start), mktime(&end));

            if (balance< 0) consec_minus++;
            else consec_minus = 0;

            if (consec_minus == 2) {not_eligible=true; break;}
            start.tm_mon++;
            end.tm_mon++;
        }
        if (not_eligible == false) account_count++;
    }
    os << bank._bank_name << "  " << account_count << "  " << total_balance << "\n";
    return os;
}