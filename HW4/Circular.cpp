#include "Circular.h"

Circular::Circular(int id, float rad) : MicroOrganism(id) {

    _radius = rad;

    _edges.push_back(new CurvyWall(2*PI*rad, rad));
}

Circular::~Circular(){

    while(!_edges.empty()){
        
        delete _edges.back();
        _edges.pop_back();
    }
}

void Circular::ConnectToCell(Cell* const cell){

    if (!_placed){

        _cell = cell;
        _placed = true;
        
        while(!_edges.empty()){

            delete _edges.back();
            _edges.pop_back();
        }

        for(int i=0; i < cell->GetCellWall().size(); i++)
            _edges.push_back(new CurvyWall( *(dynamic_cast<CurvyWall*>(cell->GetCellWall()[i]) ) ) );
    }
    
}

bool Circular::DoesFitInto(const Cell& cell) const{

    // cell.StrengthenCellWall(); const reference oldugu için bunu yapamam

    Cell test_cell(cell);
    test_cell.StrengthenCellWall();

    if (test_cell.GetCellWall().size() != 1) return false; // not a circle, clearly

    // std::cout << "cell" << cell.GetCellID() << " is a circle with radius: " << ((CurvyWall*) test_cell.GetCellWall()[0])->GetRadius();
    // only circles left alive ;)

    if ( abs( ((CurvyWall*)test_cell.GetCellWall()[0])->GetRadius() - _radius) > 0.01 ) return false;

    return true;
}

void Circular::React(){

    if (!_placed) return;

    vector<Wall*> new_wall;

    Particle old_start = _cell->GetCellWall()[0]->GetInitialParticle();
    Particle center = ((CurvyWall*)_cell->GetCellWall()[0])->GetCenter();

    double new_start_x = (old_start.X() - center.X())*2 + center.X();
    double new_start_y = (old_start.Y() - center.Y())*2 + center.Y();

    Particle new_start = Particle(new_start_x, new_start_y);

    new_wall.push_back(new CurvyWall(new_start, new_start, center));

    _cell->RenewCellWall(new_wall);
}

bool Circular::DoesContain(const Particle& particle) const{

    if(_placed && ((CurvyWall*)_cell->GetCellWall()[0])->GetCenter().FindDistance(particle) <= _radius) return true;
    return false;
}