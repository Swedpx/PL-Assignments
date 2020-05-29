#include "Triangular.h"

Triangular::Triangular(int id, const vector<Wall*>& edges, const NucleoBase& nucleobase): MicroOrganism(id, edges){
    _RNA = new NucleoBase(nucleobase);
}

Triangular::~Triangular(){
    delete _RNA;
    while(!_edges.empty()){ delete _edges.back(); _edges.pop_back();}    
}

void Triangular::ConnectToCell(Cell* const cell){

    if (!_placed){

        _cell = cell;
        _placed = true;

        while(!_edges.empty()){ delete _edges.back(); _edges.pop_back();}

        for(int i=0; i < cell->GetCellWall().size(); i++)
            _edges.push_back( new Wall( *(cell->GetCellWall()[i]) ) );
    }
}

bool Triangular::DoesFitInto(const Cell& cell) const{

    bool deleted[3] = {false};

    Cell test_cell(cell);

    test_cell.StrengthenCellWall();

    if(test_cell.GetCellWall().size() != 3) return false;

    for(int i=0; i < 3; i++){

        for(int j=0; j < 3; j++){

            if (!deleted[j] && abs(test_cell.GetCellWall()[i]->GetLength() - _edges[j]->GetLength()) < 0.01 ){

                deleted[j] = true;
                break;
            }
        }
    }
    for(int i=0; i < 3; i++) if (!deleted[i]) return false;

    return true;
}

void Triangular::React(){

    if (!_placed) return;
    _cell->GetTissue()->RemoveCell(*_cell);
    _placed = false;
    _cell = NULL;
}

const NucleoBase& Triangular::GetRNA() const{
    return *_RNA;
}

void Triangular::Mutate(Triangular& rhs){

    NucleoBase* my_tmp = _RNA;
    NucleoBase* rhs_tmp = rhs._RNA;

    NucleoBase* my_reverse_tmp = _RNA->GetLast();
    NucleoBase* rhs_reverse_tmp = rhs._RNA->GetLast();

    while(my_tmp->_value == rhs_tmp->_value){

        my_tmp = my_tmp->_next;
        rhs_tmp = rhs_tmp->_next;
    }

    while(my_reverse_tmp->_prev->_value == rhs_reverse_tmp->_prev->_value){

        my_reverse_tmp = my_reverse_tmp->_prev;
        rhs_reverse_tmp = rhs_reverse_tmp->_prev;
    }

    while(my_tmp != my_reverse_tmp && rhs_tmp != rhs_reverse_tmp){

        bool deleted = false;

        if (my_tmp->_value == 'G' && rhs_tmp->_value == 'S'){
            my_tmp->_value = 'S'; rhs_tmp->_value = 'G';

        }else if (my_tmp->_value == 'S' && rhs_tmp->_value == 'G'){
            my_tmp->_value = 'G'; rhs_tmp->_value = 'S';

        }else if(my_tmp->_value == 'A' && rhs_tmp->_value == 'U'){
            my_tmp->_value = 'U'; rhs_tmp->_value = 'A';

        }else if(my_tmp->_value == 'U' && rhs_tmp->_value == 'A'){
            my_tmp->_value = 'A'; rhs_tmp->_value = 'U';
        }
        else{
            deleted = true;

            NucleoBase* to_be_deleted = my_tmp;
            my_tmp = my_tmp->_next;

            to_be_deleted->_prev->_next = to_be_deleted->_next;
            to_be_deleted->_next->_prev = to_be_deleted->_prev;

            to_be_deleted->_prev = NULL;
            to_be_deleted->_next = NULL;
            delete to_be_deleted;

            to_be_deleted = rhs_tmp;
            rhs_tmp = rhs_tmp->_next;

            to_be_deleted->_prev->_next = to_be_deleted->_next;
            to_be_deleted->_next->_prev = to_be_deleted->_prev;

            to_be_deleted->_prev = NULL;
            to_be_deleted->_next = NULL;
            delete to_be_deleted;
        }

        if (!deleted){
            my_tmp = my_tmp->_next;
            rhs_tmp = rhs_tmp->_next;
        }
    }
}