package spock.dataaccess.ejb.support;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.BasicEntityCollection;

/**
 * Supplies a generalised set of functions for managing collections of entities within another entity.
 * @author Loren Chorley
 * @param <Econtainer> Entity container - the entity that contains the collection of entities
 * @param <E> Entity collection - the entity collection contained within the container entity
 * @param <ID> the type of the entities id field
 */
public abstract class AbstractBasicEntityCollection<Econtainer, E, ID> implements BasicEntityCollection<Econtainer, E, ID> {
    private static final Logger logger = Logger.getLogger("spock.dataaccess.ejb.support.AbstractBasicEntityCollection");
    
    protected EntityManager em;
    
    @Override
    public void setEntityManager(EntityManager em) {
        this.em = em;
    }
    
    @Override
    public E setEntityWithinCollection(Econtainer container, E entity) {
        try {
            
            Collection<E> collection = getCollection(container);
            
            ID id = getCollectionEntityID(entity);
            E e = null;
            
            if (id != null) {
                e = (E) em.find(getCollectionEntityClass(), id);
            }
            
            // If entity not found in persistence context
            if (e == null) {
                
                // Create new entity object to avoid persistence problems with already already persisted objects
                e = newCollectionEntity(id);
                
                // copy contents
                copyCollectionEntityProperties(entity, e);
                
                // add to collection
                collection.add(e);
                
                // persist
                em.persist(e);
                
            } else {
                
                // Update the entities properties
                copyCollectionEntityProperties(entity, e);
                
                // then if the collection does not contain the entity, add the entity to the collection
                if (!collection.contains(entity)) {
                    
                    collection.add(e);
                    
                }
                
            }
            
            return e;
            
        } catch (Exception ex) {
            throw new EJBException("setEntityWithinCollection threw: " + ex.getMessage());
        }
    }
    
    @Override
    public List<E> retrieveEntityWithinCollection(Econtainer container, ID id) {
        try {
            
            Collection<E> collection = getCollection(container);
            
            // if id not given, return the whole collection as a list
            // This cast is ok since the collections are implemented as array lists
            if (id == null) {
                return (List<E>) collection;
            } else {
                
                E e = (E) em.find(getCollectionEntityClass(), id);
                
                // If e is null, it was not found in the persistence context
                // therefore it cannot be in the collection, so we return null
                if (e == null) {
                    return null;
                } else {
                    
                    // If the collection contains the entity, return it in a list of one
                    if (collection.contains(e)) {
                        List<E> returnList = new ArrayList<E>();
                        returnList.add(e);
                        return returnList;
                    } else { // otherwise return null
                        return null;
                    }
                }
            }
            
        } catch (Exception ex) {
            throw new EJBException("retrieveEntityWithinCollection threw: " + ex.getMessage());
        }
    }
    
    @Override
    public Integer countEntitiesWithinCollection(Econtainer container) {
        try {
        
            return getCollection(container).size();
        
        } catch (Exception ex) {
            throw new EJBException("countEntitiesWithinCollection threw: " + ex.getMessage());
        }
    }
    
    @Override
    public void removeEntityFromCollectionByID(Econtainer container, ID id) {
        try {
            
            E e = (E) em.find(getCollectionEntityClass(), id);
            removeEntityFromCollection(container, e);
            
        } catch (Exception ex) {
            throw new EJBException("removeEntityFromCollectionByID threw: " + ex.getMessage());
        }
    }
    
    @Override
    public void removeEntityFromCollection(Econtainer container, E entity) {
        try {
            
            if (entity == null) {
                return; //TODO throw exception
            }
            
            Collection<E> collection = getCollection(container);
            
            if (collection.contains(entity)) {
                collection.remove(entity);
                return;
            } else {
                return; //TODO throw exception
            }
            
        } catch (Exception ex) {
            throw new EJBException("removeEntityFromCollection threw: " + ex.getMessage());
        }
    }
    
    protected abstract Collection<E> getCollection(Econtainer container);
    protected abstract E newCollectionEntity(ID id);
    protected abstract void copyCollectionEntityProperties(E sourceEntity, E targetEntity);
    protected abstract ID getCollectionEntityID(E entity);
    protected abstract Class getCollectionEntityClass();
    protected abstract void verifyBusinessLogic(E entity);
    
}
