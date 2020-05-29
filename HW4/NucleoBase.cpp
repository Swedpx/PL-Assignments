#include "NucleoBase.h"

NucleoBase::NucleoBase(string str, NucleoBase* prev){

    _prev = prev;
    _value = str[0];
    

    if (str.length() == 1)
        _next = NULL;

    else{
        str.erase(str.begin());
        _next = new NucleoBase(str, this);
    }
}

NucleoBase::NucleoBase(const NucleoBase& rhs){

    const NucleoBase* rhs_tmp = &rhs;
    NucleoBase* my_tmp = this;

    this->_prev = NULL;
    this->_value = rhs_tmp->_value;

    while(rhs_tmp->_next != NULL){

        if (rhs_tmp->_next != NULL)
            my_tmp->_next = new NucleoBase(string(1, rhs_tmp->_next->_value), my_tmp);

        rhs_tmp = rhs_tmp->_next;
        my_tmp = my_tmp->_next;
    }
}

NucleoBase::~NucleoBase(){

    if (this->_next != NULL)
        delete this->_next;
    
    // this->_next = NULL;
    // this->_prev = NULL;
}

NucleoBase* NucleoBase::GetLast(){

    NucleoBase* tmp = this;

    while(tmp->_next != NULL) tmp = tmp->_next;

    return tmp;
}

ostream& operator<<(ostream& os, const NucleoBase& base){

    const NucleoBase* tmp = &base;

    while(tmp != NULL){

        os << tmp->_value;
        tmp = tmp->_next;
    }
    return os;
}