#include "Tissue.h"

Tissue::Tissue(){}

Tissue::Tissue(float min_x, float max_x, float min_y, float max_y){

    _max_x = max_x; _min_x = min_x; _max_y = max_y; _min_y = min_y;
}

Tissue::~Tissue(){

    while(!_cells.empty()){
        delete _cells.back();
        _cells.pop_back();
    }
}

float* Tissue::GetCoordinates(){

    float* result = new float[4];

    result[0] = _min_x;
    result[1] = _max_x;
    result[2] = _min_y;
    result[3] = _max_y;

    return result;
}

void Tissue::AddCell(Cell& cell){

    _cells.push_back(&cell);
}

void Tissue::RemoveCell(Cell& cell){

    int i;
    bool found = false;

    for(i = 0; i < _cells.size(); i++){

        if (_cells[i]->GetCellID() == cell.GetCellID()){found = true; break;}
    }

    if (found){
        delete _cells[i];
        _cells.erase(_cells.begin() + i);
    }
}

const Cell& Tissue::GetCell(int id) const{

    bool found = false;
    int i;

    for (i=0; i < _cells.size(); i++){
        if (_cells[i]->GetCellID() == id){ found = true; break;}
    }

    if (found) return *(_cells[i]);

    else throw DeadCellException();
}

void Tissue::Interact(const vector<MicroOrganism*>& micros){


    for (int i=0; i < _cells.size(); i++){
        // std::cout << "\t\n";

        for (int j=0; j < micros.size(); j++){

            // std::cout << "is cell" << _cells[i]->GetCellID() << " compatible with MicroOrganism" << j << "?"; 

            if (micros[j]->DoesFitInto( *(_cells[i]) )){
                
                // std::cout << " Yes! To cell" << _cells[i]->GetCellID() << "\n";

                micros[j]->ConnectToCell(_cells[i]);

                break;
            }
            // std::cout << "\n";
        }
    }
}