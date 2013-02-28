/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockdataaccess.entity;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "Experiments")
@NamedQuery(name = "findAllExperiments", query = "SELECT e FROM Experiment e")
public class Experiment implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    private String id;

    public Experiment() {}
    
    public Experiment(String id, boolean isActive) {
        this.id = id;
        this.isActive = isActive;
    }
    
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    protected Boolean isActive;

    public Boolean getIsActive() {
        return isActive;
    }

    public void setIsActive(Boolean IsActive) {
        this.isActive = IsActive;
    }

    
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Experiment)) {
            return false;
        }
        Experiment other = (Experiment) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.Experiment[ id=" + id + " ]";
    }
    
}
