#include "Cell.h"

Cell::Cell(int id, const vector<Wall*>& wall, Tissue* tissue){

    _strong = wall.size() == 1;
    _id = id;
    _tissue = tissue;

    CurvyWall* test;

    for (int i=0; i < wall.size(); i++){

        // type check
        test = dynamic_cast<CurvyWall*>(wall[i]);

        if (test == NULL) // is a linear wall
            _wall.push_back( new Wall(wall[i]->GetInitialParticle(), wall[i]->GetFinalParticle()) );

        else // is a CurvyWall
            _wall.push_back( new CurvyWall(test->GetInitialParticle(), test->GetFinalParticle(), test->GetCenter()) );
    }
}

Cell::Cell(const Cell& rhs){

    _strong = rhs._strong;
    _id = rhs._id;
    _tissue = rhs._tissue;

    CurvyWall* test;

    for (int i=0; i < rhs._wall.size(); i++){

        // type check
        test = dynamic_cast<CurvyWall*>(rhs._wall[i]);

        if (test == NULL) // is a linear wall
            _wall.push_back( new Wall(rhs._wall[i]->GetInitialParticle(), rhs._wall[i]->GetFinalParticle()) );

        else // is a CurvyWall
            _wall.push_back( new CurvyWall(test->GetInitialParticle(), test->GetFinalParticle(), test->GetCenter()) );
    
    }

}


int Cell::GetCellID() const{ return _id;}

Tissue* Cell::GetTissue() const { return _tissue;}

const vector<Wall*>& Cell::GetCellWall() const { return _wall;}

void Cell::RenewCellWall(vector<Wall*>& wall){

    CurvyWall* test;

    while(!_wall.empty())  {
        delete _wall.back();
        _wall.pop_back();
    }

    for (int i=0; i < wall.size(); i++){

        // type check
        test = dynamic_cast<CurvyWall*>(wall[i]);

        if (test == NULL) // is a linear wall
            _wall.push_back( new Wall(wall[i]->GetInitialParticle(), wall[i]->GetFinalParticle()) );

        else // is a CurvyWall
            _wall.push_back( new CurvyWall(test->GetInitialParticle(), test->GetFinalParticle(), test->GetCenter()) );
    }

    while(!wall.empty()){delete wall.back(); wall.pop_back();}
}

void Cell::StrengthenCellWall(){

    vector<Wall*> tmp;
    Wall* new_wall;

    if (_strong || _wall.size() == 1) return;

    for (int i=0; i < _wall.size()-1; i++){
        
        new_wall = NULL;

        try
        {
            if (!tmp.empty()){

                try
                {
                    new_wall = (Wall*) &( *tmp.back() + *_wall[i+1] );

                    delete tmp.back();
                    tmp.pop_back();

                    tmp.push_back(new_wall);
                }
                catch(const ApplePearException& e)
                {
                    if (dynamic_cast<CurvyWall*>(_wall[i+1]) == NULL) // _wall[i+1] == Wall*
                        new_wall = new Wall(*_wall[i+1]);
                    
                    else new_wall = new CurvyWall(*dynamic_cast<CurvyWall*>(_wall[i+1]));

                    tmp.push_back(new_wall);
                }
                

            } else{
                tmp.push_back( (Wall*) &(*_wall[i] + *_wall[i+1]) );
            }
        }
        catch(const ApplePearException& e)
        {
            if (dynamic_cast<CurvyWall*>(_wall[i]) == NULL) // _wall[i] == Wall*
                new_wall = new Wall(*_wall[i]);
                    
            else new_wall = new CurvyWall(*dynamic_cast<CurvyWall*>(_wall[i]));

            tmp.push_back(new_wall);

            if (dynamic_cast<CurvyWall*>(_wall[i+1]) == NULL) // _wall[i] == Wall*
                new_wall = new Wall(*_wall[i+1]);
                    
            else new_wall = new CurvyWall(*dynamic_cast<CurvyWall*>(_wall[i+1]));

            tmp.push_back(new_wall);
        }
    }
    // what if the last and the first can be combined ?
    new_wall = NULL;
    
    if (tmp.size() > 1){

        try
        {
            new_wall = (Wall*) &( *tmp.back() + *tmp.front() );

            delete tmp.back();
            tmp.pop_back();
            
            delete tmp.front();
            tmp.erase(tmp.begin());

            tmp.insert(tmp.begin(), new_wall);
        }
        catch(const ApplePearException& e)
        {

        }
    }
    while(!_wall.empty()){
        delete _wall.back();
        _wall.pop_back();
    }
    _wall = tmp;
    _strong = true;
}

Cell::~Cell(){ 

    while(!_wall.empty()){
        delete _wall.back();
        _wall.pop_back();
    }

}

ostream& operator<<(ostream& os, const Cell& cell){

    if (!cell._wall.empty()){

        for(int i=0; i < cell._wall.size(); i++){

            if (i!=0) os << " - "; 
            os << "(" << cell._wall[i]->GetInitialParticle().X() << "," << cell._wall[i]->GetInitialParticle().Y() << ")";
        }
    }
    return os;
}
