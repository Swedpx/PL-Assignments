#include "MicroOrganism.h"

MicroOrganism::MicroOrganism(int id){

    _id = id;
    _placed = false;
    _cell = NULL;
    
}

MicroOrganism::MicroOrganism(int id, const vector<Wall*>& edges){
    
    CurvyWall* test;

    _id = id;
    _placed = false;
    _cell = NULL;

    for (int i=0; i < edges.size(); i++){
        
        test = dynamic_cast<CurvyWall*>(edges[i]);

        if (test == NULL)
                _edges.push_back(new Wall(edges[i]->GetLength()));
        
        else 
                _edges.push_back(new CurvyWall(edges[i]->GetLength(), test->GetRadius()));
        
    }
}

ostream& operator<<(ostream& os, const MicroOrganism& micro){

    if(micro._placed)
        os << "The microorganism " << micro._id << " was successfully placed into the cell " << micro._cell->GetCellID() << ".";
    else
        os << "The microorganism " << micro._id <<  " could not be placed into any cell!";
}

MicroOrganism::~MicroOrganism(){}