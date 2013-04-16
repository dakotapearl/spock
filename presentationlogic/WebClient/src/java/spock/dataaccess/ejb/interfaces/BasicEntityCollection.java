package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import java.util.List;

/**
 * Supplies a generalised set of functions for managing collections of entities within another entity.
 * @author Loren Chorley
 * @param <Econtainer> Entity container - the entity that contains the collection of entities
 * @param <E> Entity collection - the entity collection contained within the container entity
 * @param <ID> the type of the entities id field
 */
public interface BasicEntityCollection<Econtainer, E, ID> extends Serializable {
    
    /**
     * 
     * @param container
     * @param entity
     * @return 
     */
    public E setEntityWithinCollection(Econtainer container, E entity);
    
    /**
     * 
     * @param container
     * @param id
     * @return 
     */
    public List<E> retrieveEntityWithinCollection(Econtainer container, ID id);
    
    /**
     * 
     * @param container
     * @return 
     */
    public Integer countEntitiesWithinCollection(Econtainer container);
    
    /**
     * 
     * @param container
     * @param id 
     */
    public void removeEntityFromCollectionByID(Econtainer container, ID id);
    
    /**
     * 
     * @param container
     * @param entity 
     */
    public void removeEntityFromCollection(Econtainer container, E entity);
    
}
