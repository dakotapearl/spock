package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import java.util.List;

/**
 * This class determines the basic functions for an entity, though requires some basic functions to be implemented by the entity management class.
 * 
 * @author Loren Chorley
 */
public interface BasicEntity<E, ID> extends Serializable {
    
    /**
     * If no entity exists which can be identeified by the passed entities id, then it creates a new entity with this id and sets its properties to match. Otherwise it just set the properties to match, excluding the id property.
     * @param entity the entity to set or creates
     */
    public E setEntity(E entity);
    
    /**
     * if ID is set, return matching entity, null if entity doesn't exist. If ID is null, return all entities.
     * @param id
     * @return 
     */
    public List<E> retrieveEntity(ID id);
    
    /**
     * if ID is set, return matching entity, null if entity doesn't exist. If ID is null, return all entities.
     * @param id
     * @return 
     */
    public Long countEntities();
    
    /**
     * 
     * @param id 
     */
    public void removeEntityByID(ID id);
    
    /**
     * 
     * @param id
     */
    public void removeEntity(E entity);
    
}


