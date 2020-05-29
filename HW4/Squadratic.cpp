#include "Squadratic.h"

//child constructor
Squadratic::Squadratic(int id, vector<Wall*> edges, Cell* cell) : MicroOrganism(id) {

    //edges const olmadığı için sorun olabilir !
    _has_parent = true;
    _cell = cell;
    _placed = true;
    _edge_len = edges[0]->GetLength();

     for (int i=0; i < edges.size(); i++)
        _edges.push_back(edges[i]);
}

Squadratic::Squadratic(int id, float edge_len) : MicroOrganism(id) {

    _edge_len = edge_len;
    _has_parent = false;

    for (int i=0; i < 4; i++)
        _edges.push_back(new Wall(edge_len));
    
}

Squadratic::Squadratic(const Squadratic& rhs) : MicroOrganism(rhs._id, rhs._edges) {

    _has_parent = rhs._has_parent;
    _placed = rhs._placed;
    _id = rhs._id;
    _cell = rhs._cell;
    _edge_len = rhs._edge_len;

    for (int i = 0; i < rhs._childs.size(); i++)
        _childs.push_back(new Squadratic( *(rhs._childs[i])) );
    
}

Squadratic::~Squadratic(){

    while(!_edges.empty()){
        
        delete _edges.back();
        _edges.pop_back();
    }
    for (int i = 0; i < _childs.size(); i++)
        delete _childs[i];
}

void Squadratic::ConnectToCell(Cell* const cell){

    if (!_placed){
        _cell = cell;
        _placed = true;

        while(!_edges.empty()){
            delete _edges.back();
            _edges.pop_back();
        }

        for (int i=0; i < cell->GetCellWall().size(); i++)
            _edges.push_back(new Wall( *(cell->GetCellWall()[i])) );
    }
    
}

bool Squadratic::DoesFitInto(const Cell& cell) const{

    Cell test_cell(cell);

    test_cell.StrengthenCellWall();

    if (test_cell.GetCellWall().size() != 4) return false; // clearly not a square

    for (int i=0; i < 4; i++)
        if ( abs(test_cell.GetCellWall()[i]->GetLength() - _edge_len) > 0.01 ) return false;

    // açıyı kontrol etmek lazım
    for (int i=0; i < 3; i++){
        double tmp_angle = test_cell.GetCellWall()[i]->FindAngleBetween(*test_cell.GetCellWall()[i+1]);

        if ( abs(tmp_angle - 1.5708) > EPSILON && abs(tmp_angle + 1.5708) > EPSILON) return false;
    }

    return true;
}

void Squadratic::React(){

    if (!_placed) return;

    double mid_point_x;
    double mid_point_y;
    vector<Wall*> cell_walls;

    Cell strong_cell(*_cell);
    strong_cell.StrengthenCellWall();

    if (_has_parent)
        cell_walls = _edges;
    
        
    else 
        cell_walls = strong_cell.GetCellWall();
    

    vector<Particle> mid_points;

    for(int i=0; i < 4; i++){

        mid_point_x = (cell_walls[i]->GetInitialParticle().X() + cell_walls[i]->GetFinalParticle().X()) / 2;
        mid_point_y = (cell_walls[i]->GetInitialParticle().Y() + cell_walls[i]->GetFinalParticle().Y()) / 2;
        
        mid_points.push_back(Particle(mid_point_x, mid_point_y));
    }
    
    // need to add the middle of the square

    mid_point_x = (mid_points[0].X() + mid_points[2].X()) / 2;
    mid_point_y = (mid_points[0].Y() + mid_points[2].Y()) / 2;

    mid_points.push_back(Particle(mid_point_x, mid_point_y));

    vector<Wall*> walls_for_the_first;
    vector<Wall*> walls_for_the_second;
    vector<Wall*> walls_for_the_third;
    vector<Wall*> walls_for_the_fourth;

    walls_for_the_first.push_back(new Wall(cell_walls[0]->GetInitialParticle(), mid_points[0]));
    walls_for_the_first.push_back(new Wall(mid_points[0], mid_points[4]));
    walls_for_the_first.push_back(new Wall(mid_points[4], mid_points[3]));
    walls_for_the_first.push_back(new Wall(mid_points[3], cell_walls[0]->GetInitialParticle()));

    walls_for_the_second.push_back(new Wall(mid_points[0], cell_walls[0]->GetFinalParticle()));
    walls_for_the_second.push_back(new Wall(cell_walls[0]->GetFinalParticle(), mid_points[1]));
    walls_for_the_second.push_back(new Wall(mid_points[1], mid_points[4]));
    walls_for_the_second.push_back(new Wall(mid_points[4], mid_points[0]));

    walls_for_the_third.push_back(new Wall(mid_points[4], mid_points[1]));
    walls_for_the_third.push_back(new Wall(mid_points[1], cell_walls[1]->GetFinalParticle()));
    walls_for_the_third.push_back(new Wall(cell_walls[1]->GetFinalParticle(), mid_points[2]));
    walls_for_the_third.push_back(new Wall(mid_points[2], mid_points[4]));

    walls_for_the_fourth.push_back(new Wall(mid_points[3], mid_points[4]));
    walls_for_the_fourth.push_back(new Wall(mid_points[4], mid_points[2]));
    walls_for_the_fourth.push_back(new Wall(mid_points[2], cell_walls[2]->GetFinalParticle()));
    walls_for_the_fourth.push_back(new Wall(cell_walls[2]->GetFinalParticle(), mid_points[3]));

    _childs.push_back(new Squadratic(10*_id+1, walls_for_the_first, _cell));
    _childs.push_back(new Squadratic(10*_id+2, walls_for_the_second, _cell));
    _childs.push_back(new Squadratic(10*_id+3, walls_for_the_third, _cell));
    _childs.push_back(new Squadratic(10*_id+4, walls_for_the_fourth, _cell));
}

double Squadratic::getMaxX() const{

    double max_x = _edges[0]->GetInitialParticle().X();

    for(int i=0; i < 4; i++)

        if (_edges[i]->GetInitialParticle().X() > max_x) max_x = _edges[i]->GetInitialParticle().X();
    
    return max_x;
}

double Squadratic::getMaxY() const{

    double max_y = _edges[0]->GetInitialParticle().Y();

    for(int i=0; i < 4; i++)

        if (_edges[i]->GetInitialParticle().Y() > max_y) max_y = _edges[i]->GetInitialParticle().Y();
    
    return max_y;

}

double Squadratic::getMinX() const{

    double min_x = _edges[0]->GetInitialParticle().X();

    for(int i=0; i < 4; i++)

        if (_edges[i]->GetInitialParticle().X() < min_x) min_x = _edges[i]->GetInitialParticle().X();
    
    return min_x;
}

double Squadratic::getMinY() const{

    double min_y = _edges[0]->GetInitialParticle().Y();

    for(int i=0; i < 4; i++)

        if (_edges[i]->GetInitialParticle().Y() < min_y) min_y = _edges[i]->GetInitialParticle().Y();
    
    return min_y;

}

Squadratic& Squadratic::GetChild(float min_x, float max_x, float min_y, float max_y) const{

    if (!_childs.empty()){

        for (int i=0; i < 4; i++){
            Squadratic& child = *(_childs[i]);

            if (child.getMinX() == min_x && child.getMaxX() == max_x && child.getMinY() == min_y && child.getMaxY() == max_y)
                return child;
            
        }

        try{
            for (int i=0; i < 4; i++){

                Squadratic& child = *_childs[i];

                if (child.getMinX() <= min_x && child.getMaxX() >= max_x && child.getMinY() <= min_y && child.getMaxY() >= max_y)
                    return child.GetChild(min_x, max_x, min_y, max_y);
            }
        }
        catch(const NotBornChildException& e){
            throw NotBornChildException();
        }
    }
    else throw NotBornChildException();
}